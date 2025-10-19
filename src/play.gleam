import gleam/io
import gleam/list
import gleam/result
import gleam/string

import hm
import hm/ast
import hm/error
import hm/types

pub fn main() -> Nil {
  io.println("▶ Quick start example")
  run_quick_start()

  io.println("▶ Basic inference example")
  run_infer_constant()

  io.println("▶ Lambda example")
  run_infer_identity()

  io.println("▶ Let-polymorphism example")
  run_let_polymorphism()

  io.println("▶ Top-level definition example")
  run_define_function()
}

fn run_quick_start() -> Nil {
  let engine = hm.new_engine()
  let identity = ast.Lambda("x", ast.Var("x"))

  case hm.infer_expression(engine, identity) {
    Ok(#(typ, _)) -> io.println("  Inferred type: " <> format_type(typ))
    Error(reason) ->
      panic as { "Quick start failed: " <> error.describe(reason) }
  }
}

fn run_infer_constant() -> Nil {
  case infer_constant_example() {
    Ok(types.TyConst("Int")) -> io.println("  infer_constant: Int ✔")
    Ok(other) -> panic as { "Unexpected constant type: " <> format_type(other) }
    Error(reason) ->
      panic as { "infer_constant failed: " <> error.describe(reason) }
  }
}

fn run_infer_identity() -> Nil {
  case infer_identity_example() {
    Ok(types.TyFun(types.TyVar(a), types.TyVar(b))) ->
      case types.type_var_id(a) == types.type_var_id(b) {
        True -> io.println("  infer_identity: 'a -> 'a ✔")
        False -> panic as { "Identity type variables mismatch" }
      }
    Ok(other) -> panic as { "Unexpected identity type: " <> format_type(other) }
    Error(reason) ->
      panic as { "infer_identity failed: " <> error.describe(reason) }
  }
}

fn run_let_polymorphism() -> Nil {
  case let_polymorphism_example() {
    Ok(types.TyTuple([types.TyConst("Bool"), types.TyConst("Int")])) ->
      io.println("  let_polymorphism: (Bool, Int) ✔")
    Ok(other) ->
      panic as { "Unexpected let-polymorphism type: " <> format_type(other) }
    Error(reason) ->
      panic as { "let_polymorphism failed: " <> error.describe(reason) }
  }
}

fn run_define_function() -> Nil {
  case define_function_example() {
    Ok(types.TyConst("Int")) -> io.println("  define_function: Int ✔")
    Ok(other) ->
      panic as { "Unexpected define_function type: " <> format_type(other) }
    Error(reason) ->
      panic as { "define_function failed: " <> error.describe(reason) }
  }
}

fn infer_constant_example() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  extract_type(hm.infer_expression(engine, ast.Constant(ast.Int(42))))
}

fn infer_identity_example() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  extract_type(hm.infer_expression(engine, ast.Lambda("x", ast.Var("x"))))
}

fn let_polymorphism_example() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  let expr =
    ast.Let(
      "id",
      ast.Lambda("x", ast.Var("x")),
      ast.Tuple([
        ast.Apply(ast.Var("id"), ast.Constant(ast.Bool(True))),
        ast.Apply(ast.Var("id"), ast.Constant(ast.Int(42))),
      ]),
    )

  extract_type(hm.infer_expression(engine, expr))
}

fn define_function_example() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  let const_fn = ast.Lambda("x", ast.Lambda("y", ast.Var("x")))

  case hm.infer_definition(engine, "const", const_fn) {
    Ok(#(_, engine_with_const)) -> {
      let expr =
        ast.Apply(
          ast.Apply(ast.Var("const"), ast.Constant(ast.Int(5))),
          ast.Constant(ast.Bool(True)),
        )
      extract_type(hm.infer_expression(engine_with_const, expr))
    }
    Error(reason) -> Error(reason)
  }
}

fn format_type(typ: hm.Type) -> String {
  case typ {
    types.TyConst(name) -> name
    types.TyVar(types.TypeVar(_, hint)) -> "'" <> hint
    types.TyFun(from, to) ->
      "(" <> format_type(from) <> " -> " <> format_type(to) <> ")"
    types.TyList(inner) -> "[" <> format_type(inner) <> "]"
    types.TyTuple(elements) ->
      "(" <> string.join(list.map(elements, format_type), ", ") <> ")"
    types.TyCustom(name, args) ->
      name <> "(" <> string.join(list.map(args, format_type), ", ") <> ")"
    types.TyRecord(fields) -> {
      let rendered =
        list.map(fields, fn(field) {
          let #(label, field_type) = field
          label <> ": " <> format_type(field_type)
        })
      "{" <> string.join(rendered, ", ") <> "}"
    }
  }
}

fn extract_type(
  value: Result(#(hm.Type, hm.Engine), hm.InferError),
) -> Result(hm.Type, hm.InferError) {
  result.map(value, fn(pair) {
    let #(typ, _) = pair
    typ
  })
}
