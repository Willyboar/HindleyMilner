# Hindley-Milner Type Inference

A complete implementation of the Hindley-Milner type inference algorithm in Gleam. This library provides automatic type inference with let-polymorphism, algebraic data types, and rich error reporting.

## Features

- **Automatic Type Inference**: Infer types without explicit annotations
- **Let-Polymorphism**: Generalize types in let bindings for maximum reusability
- **Algebraic Data Types**: Support for enums, records, and type aliases
- **Rich Type System**: Functions, lists, tuples, records, and custom types
- **Clear Error Messages**: Descriptive error reporting for type mismatches

## Installation

Not in Hex. You need to clone it if you want to use it or add it as a github dep.
`hm = { git = "https://github.com/Willyboar/HindleyMilner.git", ref = "main" }`


## Quick Start

```gleam
import gleam/io
import gleam/list
import gleam/string
import hm
import hm/ast
import hm/error
import hm/types as types

pub fn main() -> Nil {
  let engine = hm.new_engine()
  let identity = ast.Lambda("x", ast.Var("x"))

  case hm.infer_expression(engine, identity) {
    Ok(#(typ, _)) ->
      io.println("Inferred type: " <> format_type(typ))
    Error(reason) ->
      io.println("Type error: " <> error.describe(reason))
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
    types.TyRecord(_) -> "{record}"
  }
}
```

## Core Concepts

### The Engine

The `Engine` is a stateful handle that maintains:
- A typing environment mapping variable names to type schemes
- A counter for generating fresh type variables

```gleam
// Create a new engine
let engine = hm.new_engine()

// Or create from existing parts
let engine = hm.from_parts(env, counter)
```

### Expressions

Build expressions using the `ast` module:

```gleam
import hm/ast

// Constants
let int_expr = ast.Constant(ast.Int(42))
let bool_expr = ast.Constant(ast.Bool(True))
let string_expr = ast.Constant(ast.String("hello"))

// Variables
let var_expr = ast.Var("x")

// Lambda functions: λx. x
let lambda = ast.Lambda("x", ast.Var("x"))

// Function application: f(arg)
let app = ast.Apply(function, argument)

// Let bindings: let x = value in body
let let_expr = ast.Let("x", value, body)

// Lists
let list_expr = ast.List([expr1, expr2, expr3])

// Tuples
let tuple_expr = ast.Tuple([expr1, expr2])

// Records
let record_expr = ast.Record([
  #("name", ast.Constant(ast.String("Alice"))),
  #("age", ast.Constant(ast.Int(30)))
])

// Record field access
let access = ast.RecordAccess(record_expr, "name")

// Constructor application
let some_value = ast.Constructor("Some", [ast.Constant(ast.Int(5))])
```

## Usage Examples

### Basic Type Inference

```gleam
import hm
import hm/ast

pub fn infer_constant() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  let result = hm.infer_expression(engine, ast.Constant(ast.Int(42)))
  // Returns Ok(#(TyConst("Int"), _))
  result
}
```

### Lambda Functions

```gleam
pub fn infer_identity() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  let result = hm.infer_expression(engine, ast.Lambda("x", ast.Var("x")))
  // Returns Ok(#(TyFun(_, _), _)) representing 'a -> 'a
  result
}
```

### Let-Polymorphism

Let bindings allow type generalization, enabling polymorphic reuse:

```gleam
pub fn let_polymorphism() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()

  // let id = λx. x in (id True, id 42)
  let expr = ast.Let(
    "id",
    ast.Lambda("x", ast.Var("x")),
    ast.Tuple([
      ast.Apply(ast.Var("id"), ast.Constant(ast.Bool(True))),
      ast.Apply(ast.Var("id"), ast.Constant(ast.Int(42)))
    ])
  )

  let result = hm.infer_expression(engine, expr)
  // Returns Ok(#(TyTuple([TyConst("Bool"), TyConst("Int")]), _))
  result
}
```

### Defining Top-Level Functions

Use `infer_definition` to generalize and add functions to the environment:

```gleam
pub fn define_function() {
  let engine = hm.new_engine()

  // Define: const = λx. λy. x
  let const_fn = ast.Lambda("x", ast.Lambda("y", ast.Var("x")))

  let assert Ok(#(scheme, engine)) =
    hm.infer_definition(engine, "const", const_fn)
  // scheme is: ∀a b. a -> b -> a

  // Now use it
  let expr = ast.Apply(
    ast.Apply(ast.Var("const"), ast.Constant(ast.Int(5))),
    ast.Constant(ast.Bool(True))
  )

  let assert Ok(#(typ, _)) = hm.infer_expression(engine, expr)
  // typ is: Int
}
```

### Algebraic Data Types

#### Defining Enums

```gleam
import hm/types

pub fn define_option_type() {
  let engine = hm.new_engine()

  // Define: type Option a = Some a | None
  let option_def = types.EnumType(
    name: "Option",
    params: ["a"],
    constructors: [
      types.EnumConstructor("Some", [types.ExprVar("a")]),
      types.EnumConstructor("None", [])
    ]
  )

  let assert Ok(#(engine, constructors)) =
    hm.register_definition(engine, option_def)
  // constructors = ["Some", "None"]

  // Use the constructors
  let some_int = ast.Constructor("Some", [ast.Constant(ast.Int(42))])
  let assert Ok(#(typ, _)) = hm.infer_expression(engine, some_int)
  // typ is: Option Int

  let none = ast.Constructor("None", [])
  let assert Ok(#(typ, _)) = hm.infer_expression(engine, none)
  // typ is: Option 'a (polymorphic!)
}
```

#### Defining Records

```gleam
pub fn define_record_type() {
  let engine = hm.new_engine()

  // Define: type Person = Person { name: String, age: Int }
  let person_def = types.RecordType(
    name: "Person",
    params: [],
    fields: [
      #("name", types.ExprConst("String")),
      #("age", types.ExprConst("Int"))
    ]
  )

  let assert Ok(#(engine, constructors)) =
    hm.register_definition(engine, person_def)
  // constructors = ["Person"]

  // Create a person
  let person = ast.Constructor("Person", [
    ast.Record([
      #("name", ast.Constant(ast.String("Alice"))),
      #("age", ast.Constant(ast.Int(30)))
    ])
  ])

  let assert Ok(#(typ, _)) = hm.infer_expression(engine, person)
  // typ is: Person
}
```

#### Type Aliases

```gleam
pub fn define_alias() {
  let engine = hm.new_engine()

  // Define: type IntPair = (Int, Int)
  let alias_def = types.AliasType(
    name: "IntPair",
    params: [],
    body: types.ExprTuple([
      types.ExprConst("Int"),
      types.ExprConst("Int")
    ])
  )

  let assert Ok(#(engine, _)) =
    hm.register_definition(engine, alias_def)
}
```

### Type Annotations

You can annotate expressions to enforce specific types:

```gleam
pub fn with_annotation() {
  let engine = hm.new_engine()

  // 42 : Int
  let expr = ast.Annotate(
    ast.Constant(ast.Int(42)),
    types.ExprConst("Int")
  )

  let assert Ok(#(typ, _)) = hm.infer_expression(engine, expr)
  // typ is: Int
}

pub fn annotation_mismatch() {
  let engine = hm.new_engine()

  // 42 : Bool (type error!)
  let expr = ast.Annotate(
    ast.Constant(ast.Int(42)),
    types.ExprConst("Bool")
  )

  case hm.infer_expression(engine, expr) {
    Error(error.UnificationMismatch(_, _)) -> {
      // Expected error: Int cannot unify with Bool
    }
    _ -> panic as "Should have failed!"
  }
}
```

### Working with Lists

```gleam
pub fn list_types() {
  let engine = hm.new_engine()

  // [1, 2, 3]
  let int_list = ast.List([
    ast.Constant(ast.Int(1)),
    ast.Constant(ast.Int(2)),
    ast.Constant(ast.Int(3))
  ])

  let assert Ok(#(typ, _)) = hm.infer_expression(engine, int_list)
  // typ is: List Int

  // Empty list is polymorphic
  let empty = ast.List([])
  let assert Ok(#(typ, _)) = hm.infer_expression(engine, empty)
  // typ is: List 'a
}
```

### Working with Records

```gleam
pub fn record_types() {
  let engine = hm.new_engine()

  // { x: 10, y: 20 }
  let point = ast.Record([
    #("x", ast.Constant(ast.Int(10))),
    #("y", ast.Constant(ast.Int(20)))
  ])

  let assert Ok(#(typ, _)) = hm.infer_expression(engine, point)
  // typ is: { x: Int, y: Int }

  // Access a field
  let get_x = ast.RecordAccess(point, "x")
  let assert Ok(#(typ, _)) = hm.infer_expression(engine, get_x)
  // typ is: Int
}
```

## Error Handling

The library provides detailed error types:

```gleam
import hm/error

case hm.infer_expression(engine, expr) {
  Ok(#(typ, engine)) -> {
    // Success! Use the inferred type
  }
  Error(error.UnknownIdentifier(name)) -> {
    io.println("Variable not found: " <> name)
  }
  Error(error.UnificationMismatch(left, right)) -> {
    io.println("Type mismatch!")
  }
  Error(error.OccursCheckFailed(var, typ)) -> {
    io.println("Infinite type detected!")
  }
  Error(error.UnknownConstructor(name)) -> {
    io.println("Constructor not found: " <> name)
  }
  Error(error.UnknownRecordField(record, field)) -> {
    io.println("Field not found: " <> field)
  }
  Error(error.TypeDefinitionError(message)) -> {
    io.println("Invalid type definition: " <> message)
  }
}

// Get a human-readable description
let description = error.describe(err)
```

## Advanced Usage

### Building a REPL

```gleam
import hm
import hm/ast

pub type ReplState {
  ReplState(engine: hm.Engine)
}

pub fn new_repl() -> ReplState {
  ReplState(engine: hm.new_engine())
}

pub fn eval_definition(
  repl: ReplState,
  name: String,
  expr: ast.Expr
) -> Result(#(hm.Scheme, ReplState), hm.InferError) {
  case hm.infer_definition(repl.engine, name, expr) {
    Ok(#(scheme, engine)) ->
      Ok(#(scheme, ReplState(engine: engine)))
    Error(err) -> Error(err)
  }
}

pub fn eval_expression(
  repl: ReplState,
  expr: ast.Expr
) -> Result(#(hm.Type, ReplState), hm.InferError) {
  case hm.infer_expression(repl.engine, expr) {
    Ok(#(typ, engine)) ->
      Ok(#(typ, ReplState(engine: engine)))
    Error(err) -> Error(err)
  }
}
```

### Type Checking with Prelude

```gleam
pub fn create_prelude() -> hm.Engine {
  let engine = hm.new_engine()

  // Define standard types
  let list_def = types.EnumType(
    name: "List",
    params: ["a"],
    constructors: [
      types.EnumConstructor("Cons", [
        types.ExprVar("a"),
        types.ExprCustom("List", [types.ExprVar("a")])
      ]),
      types.EnumConstructor("Nil", [])
    ]
  )

  let assert Ok(#(engine, _)) = hm.register_definition(engine, list_def)

  // Define standard functions
  let map_type = ast.Lambda("f", ast.Lambda("list",
    ast.Var("list") // Simplified
  ))

  let assert Ok(#(_, engine)) =
    hm.infer_definition(engine, "map", map_type)

  engine
}
```

## API Reference

### Main Module (`hm`)

- `new_engine() -> Engine` - Create a new inference engine
- `from_parts(TypeEnv, Counter) -> Engine` - Build engine from parts
- `environment(Engine) -> TypeEnv` - Get the environment
- `counter(Engine) -> Counter` - Get the counter
- `infer_expression(Engine, Expr) -> Result(#(Type, Engine), InferError)` - Infer expression type
- `infer_definition(Engine, String, Expr) -> Result(#(Scheme, Engine), InferError)` - Infer and generalize definition
- `register_definition(Engine, TypeDefinition) -> Result(#(Engine, List(String)), InferError)` - Register a type definition

### Expression Module (`hm/ast`)

Expression constructors for building ASTs.

### Types Module (`hm/types`)

Type representations and utilities.

### Error Module (`hm/error`)

Error types and formatting.

## Theory

This implementation follows the classic Hindley-Milner type inference algorithm with:

1. **Algorithm W**: The core inference algorithm that traverses expressions bottom-up
2. **Unification**: Robinson's unification algorithm for solving type equations
3. **Let-Polymorphism**: Automatic generalization at let bindings (rank-1 polymorphism)
4. **Principal Types**: Always infers the most general type possible

## License

[Your License Here]

## Contributing

Contributions welcome! Please open an issue or PR.
