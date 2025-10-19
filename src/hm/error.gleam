import hm/types.{type Type} as t

/// Errors that can occur while performing Hindley-Milner type inference.
pub type InferError {
  /// The occurs check failed because a type variable would contain itself.
  OccursCheckFailed(variable: t.TypeVar, in_type: Type)
  /// Two types could not be unified because their constructors disagree.
  UnificationMismatch(left: Type, right: Type)
  /// A referenced value name is missing from the environment.
  UnknownIdentifier(name: String)
  /// A requested record field is unavailable on the given record type.
  UnknownRecordField(record_type: Type, field: String)
  /// An attempt was made to use an undefined constructor.
  UnknownConstructor(name: String)
  /// A type definition was malformed or could not be registered.
  TypeDefinitionError(message: String)
}

/// Provide a concise human readable representation of an inference error.
pub fn describe(error: InferError) -> String {
  case error {
    OccursCheckFailed(_, _) ->
      "Occurs check failed while unifying a type variable"
    UnificationMismatch(_, _) -> "Unable to unify the provided types"
    UnknownIdentifier(name) -> "Unknown identifier: " <> name
    UnknownRecordField(_, field) -> "Unknown record field: " <> field
    UnknownConstructor(name) -> "Unknown constructor: " <> name
    TypeDefinitionError(message) -> "Type definition error: " <> message
  }
}
