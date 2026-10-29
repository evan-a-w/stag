use stag_ast::Prim;

use super::{error, Result, Value};

pub fn call<'p>(prim: &Prim, args: &[Value<'p>]) -> Result<Value<'p>> {
    let name = prim.name.as_bytes();
    match (name, args) {
        (b"Int.add", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a.wrapping_add(*b))),
        (b"Int.sub", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a.wrapping_sub(*b))),
        (b"Int.mul", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a.wrapping_mul(*b))),
        (b"Int.lt", [Value::Int(a), Value::Int(b)]) => Ok(Value::Bool(a < b)),
        (b"Int.equal", [Value::Int(a), Value::Int(b)]) => Ok(Value::Bool(a == b)),
        _ => error(format!(
            "not implemented: prim {} on {args:?}",
            String::from_utf8_lossy(name)
        )),
    }
}
