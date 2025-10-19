import gleam/dict.{type Dict}
import gleam/list

import hm/types

/// A mapping from type variable identifiers to concrete types.
pub type Substitution =
  Dict(Int, types.Type)

/// Create an empty substitution that performs no replacements.
pub fn empty() -> Substitution {
  dict.new()
}

/// Create a substitution that maps a single type variable to a type.
pub fn singleton(
  variable: types.TypeVar,
  replacement: types.Type,
) -> Substitution {
  dict.from_list([
    #(types.type_var_id(variable), replacement),
  ])
}

/// Merge two substitutions, applying the left-hand substitution to the values
/// stored in the right-hand substitution before combining them.
pub fn compose(left: Substitution, right: Substitution) -> Substitution {
  let updated = dict.map_values(right, fn(_, value) { apply_type(left, value) })
  dict.merge(updated, left)
}

/// Apply a substitution to every type contained within a scheme.
pub fn apply_scheme(
  substitution: Substitution,
  scheme: types.Scheme,
) -> types.Scheme {
  let types.Scheme(vars, body) = scheme
  types.Scheme(vars, apply_type(substitution, body))
}

/// Apply a substitution to a type expression recursively.
pub fn apply_type(substitution: Substitution, typ: types.Type) -> types.Type {
  case typ {
    types.TyVar(var) ->
      case dict.get(substitution, types.type_var_id(var)) {
        Ok(replacement) -> replacement
        Error(_) -> typ
      }
    types.TyConst(_) -> typ
    types.TyFun(from, to) ->
      types.TyFun(apply_type(substitution, from), apply_type(substitution, to))
    types.TyList(element) -> types.TyList(apply_type(substitution, element))
    types.TyTuple(elements) ->
      types.TyTuple(
        list.map(elements, fn(element) { apply_type(substitution, element) }),
      )
    types.TyRecord(fields) ->
      types.TyRecord(
        list.map(fields, fn(field) {
          let #(name, field_type) = field
          #(name, apply_type(substitution, field_type))
        }),
      )
    types.TyCustom(name, args) ->
      types.TyCustom(
        name,
        list.map(args, fn(arg) { apply_type(substitution, arg) }),
      )
  }
}
