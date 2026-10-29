//! A stub of the interpreter, showing its shape: evaluates a converted
//! [`typed::Program`] by walking it, borrowing it for the lifetime `'p`, with
//! "not implemented in the stub" errors for what it does not cover.

mod env;
mod external;
mod prims;
mod value;

use std::collections::HashMap;
use std::fmt;

use stag_ast::{syntax, type_, typed, Arena, Ref};
use typed::expr::Desc;
use typed::pattern::Desc as PDesc;

pub use env::Env;
pub use value::{Closure, Value};

#[derive(Debug)]
pub struct Error(pub String);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

fn error<T>(message: impl Into<String>) -> Result<T> {
    Err(Error(message.into()))
}

/// Toplevels are evaluated when first used, as by the OCaml interpreter.
enum Global<'p> {
    Pending(&'p typed::Toplevel),
    Evaluating,
    Done(Value<'p>),
}

pub struct Interp<'p> {
    arena: &'p Arena,
    globals: HashMap<&'p typed::Ident, Global<'p>>,
}

impl<'p> Interp<'p> {
    pub fn new(program: &'p typed::Program, arena: &'p Arena) -> Self {
        let globals = program
            .sccs
            .iter()
            .flat_map(|scc| &scc.members)
            .map(|toplevel| (&toplevel.ident, Global::Pending(toplevel)))
            .collect();
        Interp { arena, globals }
    }

    pub fn toplevel(&mut self, ident: &'p typed::Ident) -> Result<Value<'p>> {
        let global = self
            .globals
            .get_mut(ident)
            .ok_or_else(|| Error(format!("unknown toplevel {}", show_ident(ident))))?;
        let toplevel = match std::mem::replace(global, Global::Evaluating) {
            Global::Done(value) => {
                *global = Global::Done(value.clone());
                return Ok(value);
            }
            Global::Evaluating => {
                return error(format!(
                    "toplevel {} used while being evaluated",
                    show_ident(ident)
                ))
            }
            Global::Pending(toplevel) => toplevel,
        };
        // Recursive toplevel functions refer to themselves by [Global]s, so
        // need no name in their closures.
        let value =
            self.function_or_value(None, &toplevel.args, &toplevel.body, &Env::default())?;
        self.globals.insert(ident, Global::Done(value.clone()));
        Ok(value)
    }

    fn function_or_value(
        &mut self,
        name: Option<&'p typed::Ident>,
        args: &'p [typed::Ident],
        body: &'p typed::Expr,
        env: &Env<'p>,
    ) -> Result<Value<'p>> {
        if args.is_empty() {
            self.eval(body, env)
        } else {
            Ok(Value::closure(name, args, body, env.clone()))
        }
    }

    pub fn eval(&mut self, expr: &'p typed::Expr, env: &Env<'p>) -> Result<Value<'p>> {
        match &*expr.desc {
            Desc::Lit(lit) => Ok(Value::of_lit(lit)),
            Desc::Var(var) => self.var(expr, var, env),
            Desc::App(f, x) => {
                let f = self.eval(f, env)?;
                let x = self.eval(x, env)?;
                self.apply(f, x)
            }
            Desc::Let {
                ident,
                args,
                body,
                in_,
                ..
            } => {
                let value = self.function_or_value(Some(ident), args, body, env)?;
                self.eval(in_, &env.bind(ident, value))
            }
            Desc::Fun { args, body, .. } => Ok(Value::closure(None, args, body, env.clone())),
            Desc::Annotated(expr, _) => self.eval(expr, env),
            Desc::ToplevelSplice { expansion, .. } => self.eval(expansion, env),
            // TODO: evaluate the splices within [body], and embed the values
            // of its persistent variables, as [Treewalk]'s [build] does.
            Desc::Quote(body) => Ok(Value::Code(body)),
            Desc::Tuple(exprs) => Ok(Value::Tuple(self.eval_all(exprs, env)?)),
            Desc::Construct(constructor, exprs) => {
                Ok(Value::Construct(constructor, self.eval_all(exprs, env)?))
            }
            Desc::Record(fields) => {
                let record = match fields.first() {
                    Some((field, _)) => field.record,
                    None => return error("empty record"),
                };
                let mut values = vec![Value::Unit; self.arena[record].fields.len()];
                for (field, expr) in fields {
                    values[field.index as usize] = self.eval(expr, env)?;
                }
                Ok(Value::Record(record, values))
            }
            Desc::Field(expr, field) => match self.eval(expr, env)? {
                Value::Record(_, mut values) => Ok(values.swap_remove(field.index as usize)),
                value => error(format!("field of a non-record: {value:?}")),
            },
            Desc::Match {
                scrutinee,
                cases,
                exception_cases,
            } if exception_cases.is_empty() => {
                let value = self.eval(scrutinee, env)?;
                for (pattern, body) in cases {
                    let mut env = env.clone();
                    if matches(pattern, &value, &mut env) {
                        return self.eval(body, &env);
                    }
                }
                error(format!("match failure on {value:?}"))
            }
            _ => self.not_implemented(expr),
        }
    }

    fn eval_all(&mut self, exprs: &'p [typed::Expr], env: &Env<'p>) -> Result<Vec<Value<'p>>> {
        exprs.iter().map(|expr| self.eval(expr, env)).collect()
    }

    fn var(
        &mut self,
        expr: &'p typed::Expr,
        var: &'p typed::VarRef,
        env: &Env<'p>,
    ) -> Result<Value<'p>> {
        match var {
            typed::VarRef::Bound(typed::Binder::Local(ident)) => env
                .find(ident)
                .ok_or_else(|| Error(format!("unbound local {}", show_ident(ident)))),
            typed::VarRef::Bound(typed::Binder::Toplevel(ident)) => self.toplevel(ident),
            typed::VarRef::Prim(prim) => Ok(Value::Prim {
                prim: *prim,
                applied: Vec::new(),
            }),
            typed::VarRef::Persistent(_)
            | typed::VarRef::Free(_)
            | typed::VarRef::FreeToplevel(_)
            | typed::VarRef::Intrinsic(_) => self.not_implemented(expr),
        }
    }

    pub fn apply(&mut self, f: Value<'p>, arg: Value<'p>) -> Result<Value<'p>> {
        match f {
            Value::Closure(closure) => {
                let mut closure = (*closure).clone();
                closure.applied.push(arg);
                if closure.applied.len() < closure.args.len() {
                    return Ok(Value::Closure(closure.into()));
                }
                let mut env = closure.env.clone();
                if let Some(name) = closure.name {
                    let unapplied = Closure {
                        applied: Vec::new(),
                        ..closure.clone()
                    };
                    env = env.bind(name, Value::Closure(unapplied.into()));
                }
                for (ident, value) in closure.args.iter().zip(closure.applied) {
                    env = env.bind(ident, value);
                }
                self.eval(closure.body, &env)
            }
            Value::Prim { prim, mut applied } => {
                applied.push(arg);
                if applied.len() < self.arena[prim].args.len() {
                    Ok(Value::Prim { prim, applied })
                } else {
                    prims::call(&self.arena[prim], &applied)
                }
            }
            f => error(format!("applied a non-function: {f:?}")),
        }
    }

    fn not_implemented<T>(&self, expr: &typed::Expr) -> Result<T> {
        let shown = format!("{:?}", expr.desc);
        let node = shown
            .split(|c: char| !c.is_alphanumeric())
            .next()
            .unwrap_or("");
        error(format!(
            "not implemented in the stub: {node} (of type {}), at {}:{}",
            show_ty(self.arena, expr.ty),
            expr.loc.start.line,
            expr.loc.start.col,
        ))
    }
}

/// Binds the variables of `pattern` in `env` if it matches `value`.
fn matches<'p>(pattern: &'p typed::Pattern, value: &Value<'p>, env: &mut Env<'p>) -> bool {
    match (&*pattern.desc, value) {
        (PDesc::Any, _) => true,
        (PDesc::Var(ident), value) => {
            *env = env.bind(ident, value.clone());
            true
        }
        (PDesc::Lit(lit), value) => match (lit, value) {
            (syntax::Lit::Int(a), Value::Int(b)) => a == b,
            (syntax::Lit::Bool(a), Value::Bool(b)) => a == b,
            (syntax::Lit::String(a), Value::Str(b)) => a == b,
            (syntax::Lit::Char(a), Value::Char(b)) => a == b,
            (syntax::Lit::Unit, Value::Unit) => true,
            _ => false,
        },
        (PDesc::Tuple(patterns), Value::Tuple(values)) => matches_all(patterns, values, env),
        (PDesc::Construct(constructor, patterns), Value::Construct(c, values)) => {
            constructor.index == c.index && matches_all(patterns, values, env)
        }
        _ => false,
    }
}

fn matches_all<'p>(
    patterns: &'p [typed::Pattern],
    values: &[Value<'p>],
    env: &mut Env<'p>,
) -> bool {
    patterns.len() == values.len()
        && patterns
            .iter()
            .zip(values)
            .all(|(pattern, value)| matches(pattern, value, env))
}

pub fn show_ident(ident: &typed::Ident) -> String {
    String::from_utf8_lossy(ident.name.as_bytes()).into_owned()
}

/// Types, as available while evaluating: [`typed::Expr::ty`] in the arena.
pub fn show_ty(arena: &Arena, ty: Ref<type_::Ty>) -> String {
    let name = |s: &stag_ast::Str| String::from_utf8_lossy(s.as_bytes()).into_owned();
    match &arena[ty] {
        type_::Ty::TCon(con, args) => {
            let args: Vec<String> = args
                .iter()
                .map(|arg| match arg {
                    type_::Arg::ATy(ty) => show_ty(arena, *ty),
                    type_::Arg::ARow(_) => "_".to_owned(),
                })
                .collect();
            match args.as_slice() {
                [] => name(con),
                [arg] => format!("{arg} {}", name(con)),
                args => format!("({}) {}", args.join(", "), name(con)),
            }
        }
        type_::Ty::TArrow(a, b) => format!("({} -> {})", show_ty(arena, *a), show_ty(arena, *b)),
        type_::Ty::TTuple(tys) => {
            let tys: Vec<String> = tys.iter().map(|ty| show_ty(arena, *ty)).collect();
            format!("({})", tys.join(" * "))
        }
        type_::Ty::TCode { inner, .. } => format!("{} code", show_ty(arena, *inner)),
        type_::Ty::TRecord { .. } => "{..}".to_owned(),
        type_::Ty::TVariant { .. } => "[..]".to_owned(),
        type_::Ty::TVar(var) => match &arena[*var].state {
            type_::VarState::Link(ty) => show_ty(arena, *ty),
            type_::VarState::Unbound(_) | type_::VarState::Generic => {
                format!("'{}", arena[*var].id)
            }
        },
    }
}

impl<'p> Value<'p> {
    fn of_lit(lit: &syntax::Lit) -> Self {
        match lit {
            syntax::Lit::Int(n) => Value::Int(*n),
            syntax::Lit::Bool(b) => Value::Bool(*b),
            syntax::Lit::String(s) => Value::Str(s.clone()),
            syntax::Lit::Char(c) => Value::Char(*c),
            syntax::Lit::Unit => Value::Unit,
        }
    }
}
