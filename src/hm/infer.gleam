import gleam/dict
import gleam/list
import gleam/set
import hm/ast
import hm/env.{type TypeEnv}
import hm/error
import hm/reference
import hm/substitution.{type Substitution}
import hm/types.{
  type Scheme, type Type, collect_type_vars, instantiate_expr, type_const,
  type_fun, type_list, type_record, type_scheme, type_tuple, type_var,
  type_var_hint, type_var_id,
}
import hm/unify

/// Infer the type of an expression returning the updated counter.
pub fn infer(
  expr: ast.Expr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Type, reference.Counter), error.InferError) {
  case infer_internal(expr, environment, counter) {
    Ok(#(sub, typ, next_counter)) ->
      Ok(#(substitution.apply_type(sub, typ), next_counter))
    Error(reason) -> Error(reason)
  }
}

/// Infer the type of a top level definition and generalise it into a scheme.
pub fn infer_definition(
  name: String,
  value: ast.Expr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Scheme, TypeEnv, reference.Counter), error.InferError) {
  case infer_internal(value, environment, counter) {
    Ok(#(sub, typ, next_counter)) -> {
      let env_applied = env.apply(environment, sub)
      let scheme = generalise(env_applied, substitution.apply_type(sub, typ))
      let env_extended = env.insert(env_applied, name, scheme)
      Ok(#(scheme, env_extended, next_counter))
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_internal(
  expr: ast.Expr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case expr {
    ast.Constant(value) ->
      Ok(#(substitution.empty(), constant_type(value), counter))
    ast.Var(name) ->
      case env.lookup(environment, name) {
        Ok(scheme) -> instantiate(scheme, counter)
        Error(reason) -> Error(reason)
      }
    ast.Lambda(parameter, body) ->
      infer_lambda(parameter, body, environment, counter)
    ast.Apply(function, argument) ->
      infer_apply(function, argument, environment, counter)
    ast.Let(name, value, body) ->
      infer_let(name, value, body, environment, counter)
    ast.Annotate(inner, annotation) ->
      infer_annotation(inner, annotation, environment, counter)
    ast.List(elements) -> infer_list(elements, environment, counter)
    ast.Tuple(items) -> infer_tuple(items, environment, counter)
    ast.Record(fields) -> infer_record(fields, environment, counter)
    ast.RecordAccess(record, field) ->
      infer_record_access(record, field, environment, counter)
    ast.Constructor(name, arguments) ->
      infer_constructor(name, arguments, environment, counter)
  }
}

fn infer_lambda(
  parameter: String,
  body: ast.Expr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  let #(param_var, counter1) = reference.fresh_type_var(counter, parameter)
  let param_type = type_var(param_var)
  let env_with_param =
    env.insert(environment, parameter, type_scheme([], param_type))
  case infer_internal(body, env_with_param, counter1) {
    Ok(#(sub, body_type, counter2)) -> {
      let param_instantiated = substitution.apply_type(sub, param_type)
      let body_resolved = substitution.apply_type(sub, body_type)
      Ok(#(sub, type_fun(param_instantiated, body_resolved), counter2))
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_apply(
  function: ast.Expr,
  argument: ast.Expr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case infer_internal(function, environment, counter) {
    Ok(#(sub_fun, fun_type, counter1)) -> {
      let env_applied = env.apply(environment, sub_fun)
      case infer_internal(argument, env_applied, counter1) {
        Ok(#(sub_arg, arg_type, counter2)) -> {
          let arg_resolved = substitution.apply_type(sub_arg, arg_type)
          let result_var_hint = "res"
          let #(result_var, counter3) =
            reference.fresh_type_var(counter2, result_var_hint)
          let result_type = type_var(result_var)
          let fun_type_applied = substitution.apply_type(sub_arg, fun_type)
          let expected_type = type_fun(arg_resolved, result_type)
          case unify.unify(fun_type_applied, expected_type) {
            Ok(sub_unify) -> {
              let composed =
                substitution.compose(
                  sub_unify,
                  substitution.compose(sub_arg, sub_fun),
                )
              let final_type = substitution.apply_type(sub_unify, result_type)
              Ok(#(composed, final_type, counter3))
            }
            Error(reason) -> Error(reason)
          }
        }
        Error(reason) -> Error(reason)
      }
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_let(
  name: String,
  value: ast.Expr,
  body: ast.Expr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case infer_internal(value, environment, counter) {
    Ok(#(sub_value, value_type, counter1)) -> {
      let env_applied = env.apply(environment, sub_value)
      let generalised =
        generalise(env_applied, substitution.apply_type(sub_value, value_type))
      let env_extended = env.insert(env_applied, name, generalised)
      case infer_internal(body, env_extended, counter1) {
        Ok(#(sub_body, body_type, counter2)) -> {
          let composed = substitution.compose(sub_body, sub_value)
          let body_resolved = substitution.apply_type(sub_body, body_type)
          Ok(#(composed, body_resolved, counter2))
        }
        Error(reason) -> Error(reason)
      }
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_annotation(
  expr: ast.Expr,
  annotation: types.TypeExpr,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case infer_internal(expr, environment, counter) {
    Ok(#(sub, typ, counter1)) -> {
      case instantiate_expr(annotation, dict.new()) {
        Ok(expected) -> {
          case unify.unify(substitution.apply_type(sub, typ), expected) {
            Ok(sub_expected) ->
              Ok(#(substitution.compose(sub_expected, sub), expected, counter1))
            Error(reason) -> Error(reason)
          }
        }
        Error(message) -> Error(error.TypeDefinitionError(message))
      }
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_list(
  elements: List(ast.Expr),
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  let #(element_var, counter1) = reference.fresh_type_var(counter, "list")
  let element_type = type_var(element_var)
  let initial = #(substitution.empty(), element_type, counter1)
  case
    list.fold(elements, Ok(initial), fn(acc, element) {
      case acc {
        Error(reason) -> Error(reason)
        Ok(#(sub_acc, expected_type, counter_current)) -> {
          let env_current = env.apply(environment, sub_acc)
          case infer_internal(element, env_current, counter_current) {
            Ok(#(sub_elem, elem_type, counter_next)) -> {
              let elem_resolved = substitution.apply_type(sub_elem, elem_type)
              let expected_applied =
                substitution.apply_type(sub_elem, expected_type)
              case unify.unify(expected_applied, elem_resolved) {
                Ok(sub_match) -> {
                  let combined =
                    substitution.compose(
                      sub_match,
                      substitution.compose(sub_elem, sub_acc),
                    )
                  let updated_expected =
                    substitution.apply_type(sub_match, expected_applied)
                  Ok(#(combined, updated_expected, counter_next))
                }
                Error(reason) -> Error(reason)
              }
            }
            Error(reason) -> Error(reason)
          }
        }
      }
    })
  {
    Ok(#(sub_final, element_resolved, counter_final)) -> {
      let resolved = substitution.apply_type(sub_final, element_resolved)
      Ok(#(sub_final, type_list(resolved), counter_final))
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_tuple(
  items: List(ast.Expr),
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  let initial = #(substitution.empty(), [], counter)
  case
    list.fold(items, Ok(initial), fn(acc, item) {
      case acc {
        Error(reason) -> Error(reason)
        Ok(#(sub_acc, collected, counter_current)) -> {
          let env_current = env.apply(environment, sub_acc)
          case infer_internal(item, env_current, counter_current) {
            Ok(#(sub_item, item_type, counter_next)) -> {
              let resolved_item = substitution.apply_type(sub_item, item_type)
              let combined = substitution.compose(sub_item, sub_acc)
              Ok(#(combined, [resolved_item, ..collected], counter_next))
            }
            Error(reason) -> Error(reason)
          }
        }
      }
    })
  {
    Ok(#(sub_final, reversed_types, counter_final)) -> {
      let ordered = list.reverse(reversed_types)
      let resolved =
        list.map(ordered, fn(typ) { substitution.apply_type(sub_final, typ) })
      Ok(#(sub_final, type_tuple(resolved), counter_final))
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_record(
  fields: List(#(String, ast.Expr)),
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  let initial = #(substitution.empty(), [], counter)
  case
    list.fold(fields, Ok(initial), fn(acc, field) {
      case acc {
        Error(reason) -> Error(reason)
        Ok(#(sub_acc, collected, counter_current)) -> {
          let #(label, expr) = field
          let env_current = env.apply(environment, sub_acc)
          case infer_internal(expr, env_current, counter_current) {
            Ok(#(sub_field, field_type, counter_next)) -> {
              let resolved_field =
                substitution.apply_type(sub_field, field_type)
              let combined = substitution.compose(sub_field, sub_acc)
              Ok(#(
                combined,
                [#(label, resolved_field), ..collected],
                counter_next,
              ))
            }
            Error(reason) -> Error(reason)
          }
        }
      }
    })
  {
    Ok(#(sub_final, reversed, counter_final)) -> {
      let ordered = list.reverse(reversed)
      let resolved =
        list.map(ordered, fn(field) {
          let #(label, typ) = field
          #(label, substitution.apply_type(sub_final, typ))
        })
      Ok(#(sub_final, type_record(resolved), counter_final))
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_record_access(
  record: ast.Expr,
  field: String,
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case infer_internal(record, environment, counter) {
    Ok(#(sub_record, record_type, counter1)) -> {
      let record_resolved = substitution.apply_type(sub_record, record_type)
      case record_resolved {
        types.TyRecord(fields) ->
          case
            list.find(fields, fn(entry) {
              let #(label, _) = entry
              label == field
            })
          {
            Ok(#(_, field_type)) -> {
              let resolved = substitution.apply_type(sub_record, field_type)
              Ok(#(sub_record, resolved, counter1))
            }
            Error(_) -> Error(error.UnknownRecordField(record_resolved, field))
          }
        _ -> Error(error.UnknownRecordField(record_resolved, field))
      }
    }
    Error(reason) -> Error(reason)
  }
}

fn infer_constructor(
  name: String,
  arguments: List(ast.Expr),
  environment: TypeEnv,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case env.lookup(environment, name) {
    Ok(scheme) -> {
      case instantiate(scheme, counter) {
        Ok(#(sub_init, constructor_type, counter1)) -> {
          infer_constructor_arguments(
            arguments,
            constructor_type,
            environment,
            counter1,
            sub_init,
          )
        }
        Error(reason) -> Error(reason)
      }
    }
    Error(_) -> Error(error.UnknownConstructor(name))
  }
}

fn infer_constructor_arguments(
  arguments: List(ast.Expr),
  constructor_type: Type,
  environment: TypeEnv,
  counter: reference.Counter,
  sub_init: Substitution,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  case arguments {
    [] -> {
      let resolved = substitution.apply_type(sub_init, constructor_type)
      Ok(#(sub_init, resolved, counter))
    }
    [argument, ..rest] ->
      case constructor_type {
        types.TyFun(param_type, result_type) -> {
          case
            infer_internal(argument, env.apply(environment, sub_init), counter)
          {
            Ok(#(sub_arg, arg_type, counter1)) -> {
              let arg_resolved = substitution.apply_type(sub_arg, arg_type)
              case
                unify.unify(
                  substitution.apply_type(sub_arg, param_type),
                  arg_resolved,
                )
              {
                Ok(sub_match) -> {
                  let combined =
                    substitution.compose(
                      sub_match,
                      substitution.compose(sub_arg, sub_init),
                    )
                  infer_constructor_arguments(
                    rest,
                    substitution.apply_type(sub_match, result_type),
                    environment,
                    counter1,
                    combined,
                  )
                }
                Error(reason) -> Error(reason)
              }
            }
            Error(reason) -> Error(reason)
          }
        }
        _ -> Error(error.UnificationMismatch(constructor_type, type_tuple([])))
      }
  }
}

fn instantiate(
  scheme: Scheme,
  counter: reference.Counter,
) -> Result(#(Substitution, Type, reference.Counter), error.InferError) {
  let types.Scheme(vars, body) = scheme
  let initial = #(dict.new(), counter)
  let #(mapping, next_counter) =
    list.fold(vars, initial, fn(acc, var) {
      let #(dict_acc, current_counter) = acc
      let #(fresh_var, updated_counter) =
        reference.fresh_type_var(current_counter, type_var_hint(var))
      let dict_updated =
        dict.insert(dict_acc, type_var_id(var), type_var(fresh_var))
      #(dict_updated, updated_counter)
    })
  let substitution_map: Substitution = mapping
  Ok(#(
    substitution.empty(),
    substitution.apply_type(substitution_map, body),
    next_counter,
  ))
}

fn generalise(environment: TypeEnv, typ: Type) -> Scheme {
  let env_vars = env.free_type_vars(environment)
  let type_vars_set = types.free_type_vars(typ)
  let generic_ids = set.difference(type_vars_set, env_vars)
  let catalogue = collect_type_vars(typ)
  let ids = set.to_list(generic_ids)
  let vars =
    list.fold(ids, [], fn(acc, id) {
      case dict.get(catalogue, id) {
        Ok(var) -> [var, ..acc]
        Error(_) -> acc
      }
    })
  type_scheme(list.reverse(vars), typ)
}

fn constant_type(constant: ast.Constant) -> Type {
  case constant {
    ast.Int(_) -> type_const("Int")
    ast.Float(_) -> type_const("Float")
    ast.Bool(_) -> type_const("Bool")
    ast.String(_) -> type_const("String")
    ast.Unit -> type_const("Unit")
  }
}
