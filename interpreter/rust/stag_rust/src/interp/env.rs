use std::rc::Rc;

use stag_ast::typed;

use super::Value;

#[derive(Clone, Default)]
pub struct Env<'p>(Option<Rc<Frame<'p>>>);

struct Frame<'p> {
    ident: &'p typed::Ident,
    value: Value<'p>,
    next: Env<'p>,
}

impl<'p> Env<'p> {
    pub fn bind(&self, ident: &'p typed::Ident, value: Value<'p>) -> Self {
        Env(Some(Rc::new(Frame {
            ident,
            value,
            next: self.clone(),
        })))
    }

    pub fn find(&self, ident: &typed::Ident) -> Option<Value<'p>> {
        let mut env = self;
        while let Some(frame) = &env.0 {
            if frame.ident == ident {
                return Some(frame.value.clone());
            }
            env = &frame.next;
        }
        None
    }
}
