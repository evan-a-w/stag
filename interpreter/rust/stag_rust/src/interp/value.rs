use std::fmt;
use std::rc::Rc;

use stag_ast::{typed, Arena, InterpValue, Prim, Ref, Str};

use super::Env;

#[derive(Clone)]
pub enum Value<'p> {
    Int(i64),
    Bool(bool),
    Str(Str),
    Char(u8),
    Unit,
    Tuple(Vec<Value<'p>>),
    Construct(&'p typed::ConstructorRef, Vec<Value<'p>>),
    /// By field index.
    Record(Ref<typed::RecordType>, Vec<Value<'p>>),
    Closure(Rc<Closure<'p>>),
    Prim {
        prim: Ref<Prim>,
        applied: Vec<Value<'p>>,
    },
    Code(&'p typed::Expr),
}

#[derive(Clone)]
pub struct Closure<'p> {
    /// Bound to the closure itself in its body, for local recursive functions.
    pub name: Option<&'p typed::Ident>,
    pub args: &'p [typed::Ident],
    pub applied: Vec<Value<'p>>,
    pub body: &'p typed::Expr,
    pub env: Env<'p>,
}

impl<'p> Value<'p> {
    pub fn closure(
        name: Option<&'p typed::Ident>,
        args: &'p [typed::Ident],
        body: &'p typed::Expr,
        env: Env<'p>,
    ) -> Self {
        Value::Closure(Rc::new(Closure {
            name,
            args,
            applied: Vec::new(),
            body,
            env,
        }))
    }

    pub fn to_interp_value(&self, arena: &Arena) -> InterpValue {
        let all = |values: &[Value]| values.iter().map(|v| v.to_interp_value(arena)).collect();
        match self {
            Value::Int(n) => InterpValue::Int(*n),
            Value::Bool(b) => InterpValue::Bool(*b),
            Value::Str(s) => InterpValue::String(s.clone()),
            Value::Char(c) => InterpValue::Char(*c),
            Value::Unit => InterpValue::Unit,
            Value::Tuple(values) => InterpValue::Tuple(all(values)),
            Value::Construct(constructor, values) => {
                InterpValue::Construct(constructor.name.clone(), all(values))
            }
            Value::Record(record, values) => InterpValue::Record(
                arena[*record]
                    .fields
                    .iter()
                    .cloned()
                    .zip(values.iter().map(|v| v.to_interp_value(arena)))
                    .collect(),
            ),
            Value::Closure(_) => InterpValue::Closure,
            Value::Prim { prim, .. } => InterpValue::Prim(arena[*prim].name.clone()),
            Value::Code(expr) => InterpValue::Code((*expr).clone()),
        }
    }
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(s) => write!(f, "{s:?}"),
            Value::Char(c) => write!(f, "{:?}", *c as char),
            Value::Unit => write!(f, "()"),
            Value::Tuple(values) => f.debug_tuple("").field(values).finish(),
            Value::Construct(constructor, values) => {
                write!(
                    f,
                    "{}",
                    String::from_utf8_lossy(constructor.name.as_bytes())
                )?;
                if !values.is_empty() {
                    write!(f, " {values:?}")?;
                }
                Ok(())
            }
            Value::Record(_, values) => write!(f, "{{{values:?}}}"),
            Value::Closure(_) | Value::Prim { .. } => write!(f, "<fun>"),
            Value::Code(_) => write!(f, "<code>"),
        }
    }
}
