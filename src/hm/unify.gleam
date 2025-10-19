import gleam/list
import gleam/set
import hm/error
import hm/substitution.{type Substitution}
import hm/types.{type Type, type TypeVar, type_var_id}

/// Unify two types and return the substitution that makes them equivalent.
pub fn unify(left: Type, right: Type) -> Result(Substitution, error.InferError) {
  case left, right {
    types.TyFun(l1, r1), types.TyFun(l2, r2) -> unify_pair(l1, l2, r1, r2)
    types.TyList(a), types.TyList(b) -> unify(a, b)
    types.TyTuple(elements_a), types.TyTuple(elements_b) ->
      unify_lists(elements_a, elements_b)
    types.TyRecord(fields_a), types.TyRecord(fields_b) ->
      unify_record(fields_a, fields_b)
    types.TyCustom(name_a, args_a), types.TyCustom(name_b, args_b) ->
      case name_a == name_b {
        True -> unify_lists(args_a, args_b)
        False -> Error(error.UnificationMismatch(left, right))
      }
    types.TyConst(name_a), types.TyConst(name_b) ->
      case name_a == name_b {
        True -> Ok(substitution.empty())
        False -> Error(error.UnificationMismatch(left, right))
      }
    types.TyVar(var), other -> bind(var, other)
    other, types.TyVar(var) -> bind(var, other)
    _, _ -> Error(error.UnificationMismatch(left, right))
  }
}

fn unify_pair(
  a1: Type,
  a2: Type,
  b1: Type,
  b2: Type,
) -> Result(Substitution, error.InferError) {
  case unify(a1, a2) {
    Ok(sub1) -> {
      let b1_applied = substitution.apply_type(sub1, b1)
      let b2_applied = substitution.apply_type(sub1, b2)
      case unify(b1_applied, b2_applied) {
        Ok(sub2) -> Ok(substitution.compose(sub2, sub1))
        Error(reason) -> Error(reason)
      }
    }
    Error(reason) -> Error(reason)
  }
}

fn unify_lists(
  left: List(Type),
  right: List(Type),
) -> Result(Substitution, error.InferError) {
  case list.length(left) == list.length(right) {
    True -> fold_pairs(list.zip(left, right))
    False ->
      Error(error.UnificationMismatch(types.TyTuple(left), types.TyTuple(right)))
  }
}

fn unify_record(
  left: List(#(String, Type)),
  right: List(#(String, Type)),
) -> Result(Substitution, error.InferError) {
  case list.length(left) == list.length(right) {
    True -> {
      let zipped = list.zip(left, right)
      case
        list.all(zipped, fn(pair) {
          let #(l_field, r_field) = pair
          let #(name_a, _) = l_field
          let #(name_b, _) = r_field
          name_a == name_b
        })
      {
        True ->
          fold_pairs(
            list.map(zipped, fn(pair) {
              let #(l_field, r_field) = pair
              let #(_, left_type) = l_field
              let #(_, right_type) = r_field
              #(left_type, right_type)
            }),
          )
        False ->
          Error(error.UnificationMismatch(
            types.type_record(left),
            types.type_record(right),
          ))
      }
    }
    False ->
      Error(error.UnificationMismatch(
        types.type_record(left),
        types.type_record(right),
      ))
  }
}

fn fold_pairs(
  pairs: List(#(Type, Type)),
) -> Result(Substitution, error.InferError) {
  list.fold(pairs, Ok(substitution.empty()), fn(state, pair) {
    case state {
      Error(reason) -> Error(reason)
      Ok(sub) -> {
        let #(left, right) = pair
        let left_applied = substitution.apply_type(sub, left)
        let right_applied = substitution.apply_type(sub, right)
        case unify(left_applied, right_applied) {
          Ok(new_sub) -> Ok(substitution.compose(new_sub, sub))
          Error(reason) -> Error(reason)
        }
      }
    }
  })
}

fn bind(variable: TypeVar, typ: Type) -> Result(Substitution, error.InferError) {
  case typ {
    types.TyVar(other) ->
      case type_var_id(variable) == type_var_id(other) {
        True -> Ok(substitution.empty())
        False -> Ok(substitution.singleton(variable, typ))
      }
    _ ->
      case occurs_in(variable, typ) {
        True -> Error(error.OccursCheckFailed(variable, typ))
        False -> Ok(substitution.singleton(variable, typ))
      }
  }
}

fn occurs_in(variable: TypeVar, typ: Type) -> Bool {
  set.contains(types.free_type_vars(typ), type_var_id(variable))
}
