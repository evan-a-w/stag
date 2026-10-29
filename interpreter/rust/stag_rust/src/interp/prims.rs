use stag_ast::Prim;

use super::{error, Result, Value};

/// Calls `prim` on all its arguments. Its OCaml implementation is not
/// converted, so each is implemented again here, by name.
///
/// TODO: OCaml's ints are 63-bit, so overflow should wrap there instead.
pub fn call<'p>(prim: &Prim, args: &[Value<'p>]) -> Result<Value<'p>> {
    let name = prim.name.as_bytes();
    match (name, args) {
        (b"Int.add", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a.wrapping_add(*b))),
        (b"Int.sub", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a.wrapping_sub(*b))),
        (b"Int.mul", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a.wrapping_mul(*b))),
        (b"Int.lt", [Value::Int(a), Value::Int(b)]) => Ok(Value::Bool(a < b)),
        (b"Int.equal", [Value::Int(a), Value::Int(b)]) => Ok(Value::Bool(a == b)),
        _ => error(format!(
            "not implemented in the stub: prim {} on {args:?}",
            String::from_utf8_lossy(name)
        )),
    }
}
