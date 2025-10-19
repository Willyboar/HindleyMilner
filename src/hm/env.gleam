import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}
import hm/error
import hm/reference
import hm/substitution.{type Substitution}
import hm/types

/// The typing environment holds value bindings and registered type definitions.
pub type TypeEnv {
  TypeEnv(
    values: Dict(String, types.Scheme),
    definitions: Dict(String, types.TypeDefinition),
  )
}

/// Construct an empty typing environment.
pub fn new() -> TypeEnv {
  TypeEnv(dict.new(), dict.new())
}

/// Insert or replace a polymorphic scheme for a value name.
pub fn insert(env: TypeEnv, name: String, scheme: types.Scheme) -> TypeEnv {
  let TypeEnv(values, definitions) = env
  TypeEnv(dict.insert(values, name, scheme), definitions)
}

/// Fetch the scheme bound to the provided value name if present.
pub fn lookup(
  env: TypeEnv,
  name: String,
) -> Result(types.Scheme, error.InferError) {
  let TypeEnv(values, _) = env
  case dict.get(values, name) {
    Ok(scheme) -> Ok(scheme)
    Error(Nil) -> Error(error.UnknownIdentifier(name))
  }
}

/// Apply a substitution to every scheme stored within the environment.
pub fn apply(env: TypeEnv, sub: Substitution) -> TypeEnv {
  let TypeEnv(values, definitions) = env
  let updated =
    dict.map_values(values, fn(_name, scheme) {
      substitution.apply_scheme(sub, scheme)
    })
  TypeEnv(updated, definitions)
}

/// Retrieve the set of free type variables that remain in scope.
pub fn free_type_vars(env: TypeEnv) -> Set(Int) {
  let TypeEnv(values, _) = env
  dict.fold(values, set.new(), fn(acc, _name, scheme) {
    set.union(acc, types.free_type_vars_scheme(scheme))
  })
}

/// Register a type definition and expose any generated constructors.
pub fn register_definition(
  env: TypeEnv,
  counter: reference.Counter,
  definition: types.TypeDefinition,
) -> Result(#(TypeEnv, reference.Counter, List(String)), error.InferError) {
  case definition {
    types.AliasType(name, params, body) ->
      register_alias(env, counter, name, params, body, definition)
    types.RecordType(name, params, fields) ->
      register_record(env, counter, name, params, fields, definition)
    types.EnumType(name, params, constructors) ->
      register_enum(env, counter, name, params, constructors, definition)
  }
}

fn register_alias(
  env: TypeEnv,
  counter: reference.Counter,
  name: String,
  params: List(String),
  body: types.TypeExpr,
  definition: types.TypeDefinition,
) -> Result(#(TypeEnv, reference.Counter, List(String)), error.InferError) {
  let #(vars, next_counter) = fresh_params(counter, params)
  let mapping = parameter_mapping(params, vars)
  case instantiate_expr(body, mapping) {
    Ok(_) -> {
      let TypeEnv(values, definitions) = env
      let definitions = dict.insert(definitions, name, definition)
      Ok(#(TypeEnv(values, definitions), next_counter, []))
    }
    Error(message) -> Error(error.TypeDefinitionError(message))
  }
}

fn register_record(
  env: TypeEnv,
  counter: reference.Counter,
  name: String,
  params: List(String),
  fields: List(#(String, types.TypeExpr)),
  definition: types.TypeDefinition,
) -> Result(#(TypeEnv, reference.Counter, List(String)), error.InferError) {
  let #(vars, next_counter) = fresh_params(counter, params)
  let mapping = parameter_mapping(params, vars)
  case instantiate_fields(fields, mapping) {
    Ok(resolved_fields) -> {
      let record_type = types.type_record(resolved_fields)
      let constructor_type =
        fold_args(
          record_type,
          list.map(resolved_fields, fn(field) {
            let #(_, field_type) = field
            field_type
          }),
        )
      let scheme = types.type_scheme(vars, constructor_type)
      let TypeEnv(values, definitions) = env
      let values = dict.insert(values, name, scheme)
      let definitions = dict.insert(definitions, name, definition)
      Ok(#(TypeEnv(values, definitions), next_counter, [name]))
    }
    Error(message) -> Error(error.TypeDefinitionError(message))
  }
}

fn register_enum(
  env: TypeEnv,
  counter: reference.Counter,
  name: String,
  params: List(String),
  constructors: List(types.EnumConstructor),
  definition: types.TypeDefinition,
) -> Result(#(TypeEnv, reference.Counter, List(String)), error.InferError) {
  let #(vars, next_counter) = fresh_params(counter, params)
  let mapping = parameter_mapping(params, vars)
  let base_type =
    types.type_custom(name, list.map(vars, fn(var) { types.type_var(var) }))
  case instantiate_constructors(constructors, mapping, base_type) {
    Ok(instantiated) -> {
      let TypeEnv(values, definitions) = env
      let values =
        list.fold(instantiated, values, fn(acc, pair) {
          let #(ctor_name, ctor_type) = pair
          dict.insert(acc, ctor_name, types.type_scheme(vars, ctor_type))
        })
      let definitions = dict.insert(definitions, name, definition)
      let names =
        list.map(instantiated, fn(pair) {
          let #(ctor_name, _) = pair
          ctor_name
        })
      Ok(#(TypeEnv(values, definitions), next_counter, names))
    }
    Error(message) -> Error(error.TypeDefinitionError(message))
  }
}

fn instantiate_fields(
  fields: List(#(String, types.TypeExpr)),
  mapping: Dict(String, types.Type),
) -> Result(List(#(String, types.Type)), String) {
  list.try_map(fields, fn(field) {
    let #(label, expr) = field
    result.map(instantiate_expr(expr, mapping), fn(field_type) {
      #(label, field_type)
    })
  })
}

fn instantiate_constructors(
  constructors: List(types.EnumConstructor),
  mapping: Dict(String, types.Type),
  base: types.Type,
) -> Result(List(#(String, types.Type)), String) {
  list.try_map(constructors, fn(constructor) {
    let types.EnumConstructor(name, args) = constructor
    let resolved =
      list.try_map(args, fn(expr) { instantiate_expr(expr, mapping) })
    result.map(resolved, fn(arguments) { #(name, fold_args(base, arguments)) })
  })
}

fn instantiate_expr(
  expr: types.TypeExpr,
  mapping: Dict(String, types.Type),
) -> Result(types.Type, String) {
  case expr {
    types.ExprVar(name) ->
      case dict.get(mapping, name) {
        Ok(t) -> Ok(t)
        Error(Nil) -> Error("Unknown type parameter: " <> name)
      }
    types.ExprCustom(name, args) -> {
      use resolved_args <- result.try(
        list.try_map(args, fn(arg) { instantiate_expr(arg, mapping) }),
      )
      Ok(types.type_custom(name, resolved_args))
    }
    types.ExprFun(from, to) -> {
      use from_type <- result.try(instantiate_expr(from, mapping))
      use to_type <- result.try(instantiate_expr(to, mapping))
      Ok(types.type_fun(from_type, to_type))
    }
    _ -> Error("Unsupported type expression")
  }
}

fn fold_args(result: types.Type, args: List(types.Type)) -> types.Type {
  case args {
    [] -> result
    [first, ..rest] -> types.type_fun(first, fold_args(result, rest))
  }
}

fn fresh_params(
  counter: reference.Counter,
  params: List(String),
) -> #(List(types.TypeVar), reference.Counter) {
  let folded =
    list.fold(params, #([], counter), fn(acc, name) {
      let #(collected, current) = acc
      let #(var, next) = reference.fresh_type_var(current, name)
      #([var, ..collected], next)
    })
  let #(reversed, final_counter) = folded
  #(list.reverse(reversed), final_counter)
}

fn parameter_mapping(
  params: List(String),
  vars: List(types.TypeVar),
) -> Dict(String, types.Type) {
  let type_values = list.map(vars, fn(var) { types.type_var(var) })
  dict.from_list(list.zip(params, type_values))
}
