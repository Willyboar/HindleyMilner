import gleam/int
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should

import hm
import hm/ast
import hm/error
import hm/types

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn constant_inference_test() {
  let engine = hm.new_engine()
  let #(int_type, engine) = expect_expr(engine, ast.Constant(ast.Int(42)))
  should.equal(types.type_const("Int"), int_type)

  let #(bool_type, engine) = expect_expr(engine, ast.Constant(ast.Bool(True)))
  should.equal(types.type_const("Bool"), bool_type)

  let #(float_type, engine) = expect_expr(engine, ast.Constant(ast.Float(3.14)))
  should.equal(types.type_const("Float"), float_type)

  let #(string_type, engine) =
    expect_expr(engine, ast.Constant(ast.String("hello")))
  should.equal(types.type_const("String"), string_type)

  let #(unit_type, _) = expect_expr(engine, ast.Constant(ast.Unit))
  should.equal(types.type_const("Unit"), unit_type)
}

pub fn lambda_inference_test() {
  let engine = hm.new_engine()
  let identity = ast.Lambda("x", ast.Var("x"))
  let #(typ, _) = expect_expr(engine, identity)

  case typ {
    types.TyFun(types.TyVar(param), types.TyVar(result)) ->
      should.equal(types.type_var_id(param), types.type_var_id(result))
    other -> {
      let msg =
        "Expected a polymorphic identity function, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn nested_lambda_test() {
  let engine = hm.new_engine()
  // λx. λy. x (const function)
  let const_fn = ast.Lambda("x", ast.Lambda("y", ast.Var("x")))
  let #(typ, _) = expect_expr(engine, const_fn)

  case typ {
    types.TyFun(types.TyVar(_), types.TyFun(types.TyVar(_), types.TyVar(_))) ->
      Nil
    other -> {
      let msg = "Expected a -> b -> a, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn application_inference_test() {
  let engine = hm.new_engine()
  let expr = ast.Apply(ast.Lambda("x", ast.Var("x")), ast.Constant(ast.Int(10)))

  let #(typ, _) = expect_expr(engine, expr)
  should.equal(types.type_const("Int"), typ)
}

pub fn nested_application_test() {
  let engine = hm.new_engine()
  // (λx. λy. x) 5 True = 5
  let const_fn = ast.Lambda("x", ast.Lambda("y", ast.Var("x")))
  let expr =
    ast.Apply(
      ast.Apply(const_fn, ast.Constant(ast.Int(5))),
      ast.Constant(ast.Bool(True)),
    )

  let #(typ, _) = expect_expr(engine, expr)
  should.equal(types.type_const("Int"), typ)
}

pub fn let_generalisation_test() {
  let engine = hm.new_engine()
  let id_lambda = ast.Lambda("x", ast.Var("x"))
  let expr =
    ast.Let(
      "id",
      id_lambda,
      ast.Tuple([
        ast.Apply(ast.Var("id"), ast.Constant(ast.Bool(True))),
        ast.Apply(ast.Var("id"), ast.Constant(ast.Int(0))),
      ]),
    )

  let #(typ, _) = expect_expr(engine, expr)
  case typ {
    types.TyTuple([first, second]) -> {
      should.equal(types.type_const("Bool"), first)
      should.equal(types.type_const("Int"), second)
    }
    other -> {
      let msg = "Expected a tuple result, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn nested_let_test() {
  let engine = hm.new_engine()
  // let x = 5 in let y = x in y
  let expr =
    ast.Let(
      "x",
      ast.Constant(ast.Int(5)),
      ast.Let("y", ast.Var("x"), ast.Var("y")),
    )

  let #(typ, _) = expect_expr(engine, expr)
  should.equal(types.type_const("Int"), typ)
}

pub fn shadowing_test() {
  let engine = hm.new_engine()
  // let x = 5 in let x = True in x
  let expr =
    ast.Let(
      "x",
      ast.Constant(ast.Int(5)),
      ast.Let("x", ast.Constant(ast.Bool(True)), ast.Var("x")),
    )

  let #(typ, _) = expect_expr(engine, expr)
  should.equal(types.type_const("Bool"), typ)
}

pub fn list_inference_test() {
  let engine = hm.new_engine()
  let #(typ, _) =
    expect_expr(
      engine,
      ast.List([ast.Constant(ast.Int(1)), ast.Constant(ast.Int(2))]),
    )

  case typ {
    types.TyList(element) -> should.equal(types.type_const("Int"), element)
    other -> {
      let msg = "Expected a list type, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn empty_list_polymorphism_test() {
  let engine = hm.new_engine()
  let #(typ, _) = expect_expr(engine, ast.List([]))

  case typ {
    types.TyList(types.TyVar(_)) -> Nil
    other -> {
      let msg = "Expected a polymorphic list type, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn heterogeneous_list_error_test() {
  let engine = hm.new_engine()
  let expr = ast.List([ast.Constant(ast.Int(1)), ast.Constant(ast.Bool(True))])

  case hm.infer_expression(engine, expr) {
    Error(error.UnificationMismatch(_, _)) -> Nil
    Ok(_) -> {
      should.fail()
      panic as "Expected heterogeneous list to fail"
    }
    Error(other) -> {
      let msg = "Unexpected error: " <> error.describe(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn tuple_inference_test() {
  let engine = hm.new_engine()
  let expr =
    ast.Tuple([
      ast.Constant(ast.Int(1)),
      ast.Constant(ast.Bool(True)),
      ast.Constant(ast.String("hello")),
    ])

  let #(typ, _) = expect_expr(engine, expr)
  case typ {
    types.TyTuple([first, second, third]) -> {
      should.equal(types.type_const("Int"), first)
      should.equal(types.type_const("Bool"), second)
      should.equal(types.type_const("String"), third)
    }
    other -> {
      let msg = "Expected a 3-tuple, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn empty_tuple_test() {
  let engine = hm.new_engine()
  let #(typ, _) = expect_expr(engine, ast.Tuple([]))

  case typ {
    types.TyTuple([]) -> Nil
    other -> {
      let msg = "Expected empty tuple, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn record_inference_test() {
  let engine = hm.new_engine()
  let record_expr =
    ast.Record([
      #("x", ast.Constant(ast.Int(4))),
      #("y", ast.Constant(ast.Bool(False))),
    ])

  let #(typ, _) = expect_expr(engine, record_expr)

  case typ {
    types.TyRecord(fields) -> {
      case fields {
        [#("x", x_type), #("y", y_type)] -> {
          should.equal(types.type_const("Int"), x_type)
          should.equal(types.type_const("Bool"), y_type)
        }
        _ -> {
          should.fail()
          panic as "Record fields were stored in an unexpected order"
        }
      }
    }
    other -> {
      let msg = "Expected a record type, got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn record_access_inference_test() {
  let engine = hm.new_engine()
  let record_expr =
    ast.RecordAccess(
      ast.Record([
        #("x", ast.Constant(ast.Int(7))),
        #("y", ast.Constant(ast.Bool(True))),
      ]),
      "y",
    )

  let #(typ, _) = expect_expr(engine, record_expr)
  should.equal(types.type_const("Bool"), typ)
}

pub fn record_access_unknown_field_test() {
  let engine = hm.new_engine()
  let record_expr =
    ast.RecordAccess(ast.Record([#("x", ast.Constant(ast.Int(7)))]), "unknown")

  case hm.infer_expression(engine, record_expr) {
    Error(error.UnknownRecordField(_, field)) -> should.equal("unknown", field)
    Ok(_) -> {
      should.fail()
      panic as "Expected unknown field error"
    }
    Error(other) -> {
      let msg = "Unexpected error: " <> error.describe(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn constructor_inference_test() {
  let engine = hm.new_engine()
  let option_definition =
    types.EnumType(name: "Option", params: ["a"], constructors: [
      types.EnumConstructor("Some", [types.ExprVar("a")]),
      types.EnumConstructor("None", []),
    ])

  let assert Ok(#(engine, constructors)) =
    hm.register_definition(engine, option_definition)

  // Just verify we got 2 constructors back
  should.equal(2, list.length(constructors))

  // Try to use the constructors
  let #(some_type, engine) =
    expect_expr(engine, ast.Constructor("Some", [ast.Constant(ast.Int(5))]))
  case some_type {
    types.TyCustom("Option", [types.TyConst("Int")]) -> Nil
    other -> {
      panic as string.concat([
          "Unexpected constructor type: ",
          inspect_type(other),
        ])
    }
  }

  let #(none_type, _) = expect_expr(engine, ast.Constructor("None", []))
  case none_type {
    types.TyCustom("Option", [types.TyVar(_)]) -> Nil
    other -> {
      panic as string.concat([
          "Unexpected None constructor type: ",
          inspect_type(other),
        ])
    }
  }
}

pub fn maybe_list_constructor_test() {
  let engine = hm.new_engine()
  // Define a list type from scratch
  let list_def =
    types.EnumType(name: "List", params: ["a"], constructors: [
      types.EnumConstructor("Cons", [
        types.ExprVar("a"),
        types.ExprCustom("List", [types.ExprVar("a")]),
      ]),
      types.EnumConstructor("Nil", []),
    ])

  let assert Ok(#(engine, constructors)) =
    hm.register_definition(engine, list_def)
  should.equal(2, list.length(constructors))

  // Cons 5 Nil
  let expr =
    ast.Constructor("Cons", [
      ast.Constant(ast.Int(5)),
      ast.Constructor("Nil", []),
    ])

  let #(typ, _) = expect_expr(engine, expr)
  case typ {
    types.TyCustom("List", [types.TyConst("Int")]) -> Nil
    other -> {
      panic as string.concat([
          "Unexpected list constructor type: ",
          inspect_type(other),
        ])
    }
  }
}

pub fn unknown_constructor_test() {
  let engine = hm.new_engine()
  let expr = ast.Constructor("Unknown", [])

  case hm.infer_expression(engine, expr) {
    Error(error.UnknownConstructor(name)) -> should.equal("Unknown", name)
    Ok(_) -> {
      should.fail()
      panic as "Expected unknown constructor error"
    }
    Error(other) -> {
      let msg = "Unexpected error: " <> error.describe(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn annotation_success_test() {
  let engine = hm.new_engine()
  let annotated = ast.Annotate(ast.Constant(ast.Int(4)), types.ExprConst("Int"))

  let #(typ, _) = expect_expr(engine, annotated)
  should.equal(types.type_const("Int"), typ)
}

pub fn annotation_failure_test() {
  let engine = hm.new_engine()
  let annotated =
    ast.Annotate(ast.Constant(ast.Int(4)), types.ExprConst("Bool"))

  case hm.infer_expression(engine, annotated) {
    Ok(_) -> {
      should.fail()
      panic as "Expected the annotation to fail, yet it succeeded"
    }
    Error(error.UnificationMismatch(_, _)) -> Nil
    Error(other) -> {
      let msg = "Unexpected error: " <> error.describe(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn annotation_polymorphic_test() {
  let engine = hm.new_engine()
  // Just annotate with a concrete type instead
  let annotated =
    ast.Annotate(
      ast.Lambda("x", ast.Var("x")),
      types.ExprFun(types.ExprConst("Int"), types.ExprConst("Int")),
    )

  let #(typ, _) = expect_expr(engine, annotated)
  should.equal(
    types.type_fun(types.type_const("Int"), types.type_const("Int")),
    typ,
  )
}

pub fn definition_generalisation_test() {
  let engine = hm.new_engine()
  let id_lambda = ast.Lambda("x", ast.Var("x"))
  let #(scheme, engine) = expect_definition(engine, "id", id_lambda)

  let #(application_type, _) =
    expect_expr(engine, ast.Apply(ast.Var("id"), ast.Constant(ast.Bool(True))))

  should.equal(types.type_const("Bool"), application_type)

  case scheme {
    types.Scheme(vars, types.TyFun(types.TyVar(arg), types.TyVar(result))) -> {
      should.equal(1, list.length(vars))
      should.equal(types.type_var_id(arg), types.type_var_id(result))
    }
    other -> {
      let msg = "Expected a generalised scheme, got " <> inspect_scheme(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn multiple_definitions_test() {
  let engine = hm.new_engine()

  // Define const: λx. λy. x
  let const_fn = ast.Lambda("x", ast.Lambda("y", ast.Var("x")))
  let #(_, engine) = expect_definition(engine, "const", const_fn)

  // Define id: λx. x
  let id_fn = ast.Lambda("x", ast.Var("x"))
  let #(_, engine) = expect_definition(engine, "id", id_fn)

  // Use both: const (id 5) True
  let expr =
    ast.Apply(
      ast.Apply(
        ast.Var("const"),
        ast.Apply(ast.Var("id"), ast.Constant(ast.Int(5))),
      ),
      ast.Constant(ast.Bool(True)),
    )

  let #(typ, _) = expect_expr(engine, expr)
  should.equal(types.type_const("Int"), typ)
}

pub fn recursive_type_definition_test() {
  let engine = hm.new_engine()

  // type Tree a = Leaf a | Node (Tree a) (Tree a)
  let tree_def =
    types.EnumType(name: "Tree", params: ["a"], constructors: [
      types.EnumConstructor("Leaf", [types.ExprVar("a")]),
      types.EnumConstructor("Node", [
        types.ExprCustom("Tree", [types.ExprVar("a")]),
        types.ExprCustom("Tree", [types.ExprVar("a")]),
      ]),
    ])

  let assert Ok(#(engine, constructors)) =
    hm.register_definition(engine, tree_def)
  should.equal(2, list.length(constructors))

  // Node (Leaf 1) (Leaf 2)
  let expr =
    ast.Constructor("Node", [
      ast.Constructor("Leaf", [ast.Constant(ast.Int(1))]),
      ast.Constructor("Leaf", [ast.Constant(ast.Int(2))]),
    ])

  let #(typ, _) = expect_expr(engine, expr)
  case typ {
    types.TyCustom("Tree", [types.TyConst("Int")]) -> Nil
    other -> {
      panic as string.concat([
          "Unexpected tree constructor type: ",
          inspect_type(other),
        ])
    }
  }
}

pub fn complex_expression_test() {
  let engine = hm.new_engine()

  // let add = λx. λy. x in
  // let pair = (add 1, add True) in
  // pair
  let expr =
    ast.Let(
      "add",
      ast.Lambda("x", ast.Lambda("y", ast.Var("x"))),
      ast.Let(
        "pair",
        ast.Tuple([
          ast.Apply(ast.Var("add"), ast.Constant(ast.Int(1))),
          ast.Apply(ast.Var("add"), ast.Constant(ast.Bool(True))),
        ]),
        ast.Var("pair"),
      ),
    )

  let #(typ, _) = expect_expr(engine, expr)
  case typ {
    types.TyTuple([
      types.TyFun(types.TyVar(_), types.TyConst("Int")),
      types.TyFun(types.TyVar(_), types.TyConst("Bool")),
    ]) -> Nil
    other -> {
      let msg = "Expected (b -> Int, c -> Bool), got " <> inspect_type(other)
      should.fail()
      panic as msg
    }
  }
}

pub fn unknown_variable_test() {
  let engine = hm.new_engine()
  let expr = ast.Var("unknown")

  case hm.infer_expression(engine, expr) {
    Error(error.UnknownIdentifier(name)) -> should.equal("unknown", name)
    Ok(_) -> {
      should.fail()
      panic as "Expected unknown identifier error"
    }
    Error(other) -> {
      let msg = "Unexpected error: " <> error.describe(other)
      should.fail()
      panic as msg
    }
  }
}

fn expect_expr(engine: hm.Engine, expr: hm.Expr) -> #(hm.Type, hm.Engine) {
  case hm.infer_expression(engine, expr) {
    Ok(value) -> value
    Error(reason) -> {
      let msg = error.describe(reason)
      should.fail()
      panic as msg
    }
  }
}

fn expect_definition(
  engine: hm.Engine,
  name: String,
  expr: hm.Expr,
) -> #(hm.Scheme, hm.Engine) {
  case hm.infer_definition(engine, name, expr) {
    Ok(value) -> value
    Error(reason) -> {
      let msg = error.describe(reason)
      should.fail()
      panic as msg
    }
  }
}

fn inspect_type(typ: hm.Type) -> String {
  case typ {
    types.TyConst(name) -> "Const " <> name
    types.TyVar(types.TypeVar(id, hint)) ->
      "Var " <> hint <> "(" <> int.to_string(id) <> ")"
    types.TyFun(_, _) -> "Function"
    types.TyList(_) -> "List"
    types.TyTuple(_) -> "Tuple"
    types.TyRecord(_) -> "Record"
    types.TyCustom(name, _) -> "Custom " <> name
  }
}

fn inspect_scheme(scheme: hm.Scheme) -> String {
  case scheme {
    types.Scheme(vars, _) ->
      "Scheme with " <> int.to_string(list.length(vars)) <> " vars"
  }
}
