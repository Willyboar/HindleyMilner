import gleam/dict.{type Dict}

//import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}

import gleam/string

/// A unique type variable identified by a numeric id and an optional hint for
/// readability in error messages.
pub type TypeVar {
  TypeVar(id: Int, hint: String)
}

/// Extract the integer identifier from a type variable.
pub fn type_var_id(var: TypeVar) -> Int {
  let TypeVar(id, _) = var
  id
}

/// Read the human friendly hint associated with a type variable.
pub fn type_var_hint(var: TypeVar) -> String {
  let TypeVar(_, hint) = var
  hint
}

/// A Hindley-Milner type used by the inference engine to describe expressions.
pub type Type {
  /// A type variable that may later be unified with another type.
  TyVar(TypeVar)
  /// A named primitive type such as `Int`, `Float`, or user defined constants.
  TyConst(name: String)
  /// A function type from one type to another.
  TyFun(from: Type, to: Type)
  /// A homogenous list of elements sharing the same type.
  TyList(element: Type)
  /// A tuple grouping a fixed number of typed values.
  TyTuple(elements: List(Type))
  /// A record with labelled fields storing their associated types.
  TyRecord(fields: List(#(String, Type)))
  /// A user defined type constructor applied to zero or more arguments.
  TyCustom(name: String, args: List(Type))
}

/// A universally quantified type scheme describing polymorphic definitions.
pub type Scheme {
  Scheme(vars: List(TypeVar), body: Type)
}

/// A constructor within an algebraic data type definition.
pub type EnumConstructor {
  EnumConstructor(name: String, args: List(TypeExpr))
}

/// The syntactic description of types used when defining new type aliases or
/// data structures.
pub type TypeExpr {
  /// A reference to a type parameter identified by name.
  ExprVar(name: String)
  /// A reference to a named type constructor.
  ExprConst(name: String)
  /// A function type composed of an argument and return expression.
  ExprFun(argument: TypeExpr, result: TypeExpr)
  /// A list type wrapping the element expression.
  ExprList(element: TypeExpr)
  /// A tuple of nested type expressions.
  ExprTuple(elements: List(TypeExpr))
  /// A record literal expressed through a list of labelled fields.
  ExprRecord(fields: List(#(String, TypeExpr)))
  /// A user defined type constructor applied to argument expressions.
  ExprCustom(name: String, args: List(TypeExpr))
}

/// An algebraic data type, record, or alias that may be registered with the
/// inference environment.
pub type TypeDefinition {
  /// An alias replaces the definition name with the provided body expression.
  AliasType(name: String, params: List(String), body: TypeExpr)
  /// A record definition provides a named collection of typed fields.
  RecordType(
    name: String,
    params: List(String),
    fields: List(#(String, TypeExpr)),
  )
  /// An enum definition introduces one or more constructors.
  EnumType(
    name: String,
    params: List(String),
    constructors: List(EnumConstructor),
  )
}

/// Create a type variable term from an existing variable record.
pub fn type_var(var: TypeVar) -> Type {
  TyVar(var)
}

/// Construct a primitive or user defined constant type by name.
pub fn type_const(name: String) -> Type {
  TyConst(name)
}

/// Construct a function type from an argument type to a return type.
pub fn type_fun(from: Type, to: Type) -> Type {
  TyFun(from, to)
}

/// Construct a list type for the provided element type.
pub fn type_list(element: Type) -> Type {
  TyList(element)
}

/// Construct a tuple type, normalising the element order for determinism.
pub fn type_tuple(elements: List(Type)) -> Type {
  TyTuple(elements)
}

/// Construct a record type from the supplied fields, sorted by label.
pub fn type_record(fields: List(#(String, Type))) -> Type {
  TyRecord(
    list.sort(fields, fn(a, b) {
      let #(name_a, _) = a
      let #(name_b, _) = b
      string.compare(name_a, name_b)
    }),
  )
}

/// Construct a type constructor application from a name and argument list.
pub fn type_custom(name: String, args: List(Type)) -> Type {
  TyCustom(name, args)
}

/// Build a type scheme from its bound variables and underlying type.
pub fn type_scheme(vars: List(TypeVar), body: Type) -> Scheme {
  Scheme(vars, body)
}

/// Compute the free type variables contained within a type.
pub fn free_type_vars(typ: Type) -> Set(Int) {
  free_type_vars_acc(typ, set.new())
}

/// Compute the free type variables contained within a scheme.
pub fn free_type_vars_scheme(scheme: Scheme) -> Set(Int) {
  let Scheme(vars, body) = scheme
  let body_vars = free_type_vars(body)
  list.fold(vars, body_vars, fn(acc, var) { set.delete(acc, type_var_id(var)) })
}

/// Instantiate a syntactic type expression by replacing named parameters using
/// the supplied dictionary.
pub fn instantiate_expr(
  expr: TypeExpr,
  mapping: Dict(String, Type),
) -> Result(Type, String) {
  case expr {
    ExprVar(name) -> {
      case dict.get(mapping, name) {
        Ok(value) -> Ok(value)
        Error(_) -> Error("Unknown type parameter: " <> name)
      }
    }
    ExprConst(name) -> Ok(type_const(name))
    ExprFun(argument, output) -> {
      case instantiate_expr(argument, mapping) {
        Ok(argument_type) ->
          result.map(instantiate_expr(output, mapping), fn(result_type) {
            type_fun(argument_type, result_type)
          })
        Error(error) -> Error(error)
      }
    }
    ExprList(element) ->
      result.map(instantiate_expr(element, mapping), type_list)
    ExprTuple(elements) -> {
      let recursive =
        list.try_map(elements, fn(element_expr) {
          instantiate_expr(element_expr, mapping)
        })
      result.map(recursive, type_tuple)
    }
    ExprRecord(fields) -> {
      let helper =
        list.try_map(fields, fn(field) {
          let #(name, expr) = field
          result.map(instantiate_expr(expr, mapping), fn(value) {
            #(name, value)
          })
        })
      result.map(helper, type_record)
    }
    ExprCustom(name, args) -> {
      let helper =
        list.try_map(args, fn(expr) { instantiate_expr(expr, mapping) })
      result.map(helper, fn(instantiated) { type_custom(name, instantiated) })
    }
  }
}

/// Collect the type variables contained within a type indexed by identifier.
pub fn collect_type_vars(typ: Type) -> Dict(Int, TypeVar) {
  collect_type_vars_acc(typ, dict.new())
}

fn free_type_vars_acc(typ: Type, acc: Set(Int)) -> Set(Int) {
  case typ {
    TyVar(var) -> set.insert(acc, type_var_id(var))
    TyConst(_) -> acc
    TyFun(from, to) -> {
      let with_from = free_type_vars_acc(from, acc)
      free_type_vars_acc(to, with_from)
    }
    TyList(element) -> free_type_vars_acc(element, acc)
    TyTuple(elements) ->
      list.fold(elements, acc, fn(state, element) {
        free_type_vars_acc(element, state)
      })
    TyRecord(fields) ->
      list.fold(fields, acc, fn(state, field) {
        let #(_, field_type) = field
        free_type_vars_acc(field_type, state)
      })
    TyCustom(_, args) ->
      list.fold(args, acc, fn(state, arg) { free_type_vars_acc(arg, state) })
  }
}

fn collect_type_vars_acc(
  typ: Type,
  acc: Dict(Int, TypeVar),
) -> Dict(Int, TypeVar) {
  case typ {
    TyVar(var) -> dict.insert(acc, type_var_id(var), var)
    TyConst(_) -> acc
    TyFun(from, to) ->
      collect_type_vars_acc(to, collect_type_vars_acc(from, acc))
    TyList(element) -> collect_type_vars_acc(element, acc)
    TyTuple(elements) ->
      list.fold(elements, acc, fn(state, element) {
        collect_type_vars_acc(element, state)
      })
    TyRecord(fields) ->
      list.fold(fields, acc, fn(state, field) {
        let #(_, field_type) = field
        collect_type_vars_acc(field_type, state)
      })
    TyCustom(_, args) ->
      list.fold(args, acc, fn(state, arg) { collect_type_vars_acc(arg, state) })
  }
}
