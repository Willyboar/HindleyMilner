import hm/ast
import hm/env
import hm/error
import hm/infer
import hm/reference
import hm/types

/// Re-export of the surface language expression type.
pub type Expr =
  ast.Expr

/// Re-export of supported literal constants.
pub type Constant =
  ast.Constant

/// Re-export of the principal type representation.
pub type Type =
  types.Type

/// Re-export of universally quantified schemes.
pub type Scheme =
  types.Scheme

/// Re-export of algebraic type constructors.
pub type EnumConstructor =
  types.EnumConstructor

/// Re-export of type expressions used when declaring new types.
pub type TypeExpr =
  types.TypeExpr

/// Re-export of full type definitions.
pub type TypeDefinition =
  types.TypeDefinition

/// Re-export of inference errors.
pub type InferError =
  error.InferError

/// Mutable-style handle that threads the typing environment and fresh counter.
pub type Engine {
  Engine(env: env.TypeEnv, counter: reference.Counter)
}

/// Construct a new inference engine with an empty environment.
pub fn new_engine() -> Engine {
  Engine(env.new(), reference.new_counter(0))
}

/// Build an inference engine from an explicit environment and counter.
pub fn from_parts(env: env.TypeEnv, counter: reference.Counter) -> Engine {
  Engine(env, counter)
}

/// Access the current environment stored within the engine.
pub fn environment(engine: Engine) -> env.TypeEnv {
  let Engine(env, _) = engine
  env
}

/// Access the fresh counter stored within the engine.
pub fn counter(engine: Engine) -> reference.Counter {
  let Engine(_, counter) = engine
  counter
}

/// Register a type definition, returning an updated engine and any constructor
/// names introduced by the definition.
pub fn register_definition(
  engine: Engine,
  definition: TypeDefinition,
) -> Result(#(Engine, List(String)), InferError) {
  let Engine(current_env, current_counter) = engine
  case env.register_definition(current_env, current_counter, definition) {
    Ok(#(next_env, next_counter, constructors)) ->
      Ok(#(Engine(next_env, next_counter), constructors))
    Error(reason) -> Error(reason)
  }
}

/// Infer the type of an expression, returning the updated engine alongside the
/// inferred type.
pub fn infer_expression(
  engine: Engine,
  expr: Expr,
) -> Result(#(Type, Engine), InferError) {
  let Engine(current_env, current_counter) = engine
  case infer.infer(expr, current_env, current_counter) {
    Ok(#(typ, next_counter)) -> Ok(#(typ, Engine(current_env, next_counter)))
    Error(reason) -> Error(reason)
  }
}

/// Infer and generalise a top level definition, extending the engine's
/// environment with the resulting scheme.
pub fn infer_definition(
  engine: Engine,
  name: String,
  expr: Expr,
) -> Result(#(Scheme, Engine), InferError) {
  let Engine(current_env, current_counter) = engine
  case infer.infer_definition(name, expr, current_env, current_counter) {
    Ok(#(scheme, next_env, next_counter)) ->
      Ok(#(scheme, Engine(next_env, next_counter)))
    Error(reason) -> Error(reason)
  }
}

/// Convenience constructor for type environments without creating an engine.
pub fn empty_env() -> env.TypeEnv {
  env.new()
}
