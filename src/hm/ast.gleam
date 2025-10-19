import hm/types

/// Surface language constants supported by the inference engine.
pub type Constant {
  /// A signed integer literal.
  Int(Int)
  /// A 64-bit floating point literal.
  Float(Float)
  /// A boolean literal.
  Bool(Bool)
  /// A UTF-8 string literal.
  String(String)
  /// The unit value.
  Unit
}

/// Expressions accepted by the inference engine.
pub type Expr {
  /// A literal constant.
  Constant(Constant)
  /// A reference to a name in the current environment.
  Var(String)
  /// A function abstraction binding a single argument.
  Lambda(parameter: String, body: Expr)
  /// A function application.
  Apply(function: Expr, argument: Expr)
  /// A non-recursive let binding.
  Let(name: String, value: Expr, body: Expr)
  /// An explicit type annotation.
  Annotate(expr: Expr, annotation: types.TypeExpr)
  /// A list literal.
  List(List(Expr))
  /// A tuple literal.
  Tuple(List(Expr))
  /// A record literal constructed from labelled fields.
  Record(List(#(String, Expr)))
  /// Access a record field by label.
  RecordAccess(record: Expr, field: String)
  /// Construct a value using a registered data constructor.
  Constructor(name: String, arguments: List(Expr))
}
