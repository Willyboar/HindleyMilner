import hm
import hm/ast

pub fn infer_constant() -> Result(hm.Type, hm.InferError) {
  let engine = hm.new_engine()
  let result = hm.infer_expression(engine, ast.Constant(ast.Int(42)))
  case result {
    Ok(#(ty, _)) -> Ok(ty)
    Error(e) -> Error(e)
  }
}
