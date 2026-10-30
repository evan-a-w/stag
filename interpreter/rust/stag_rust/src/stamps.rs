use ocaml_sys::{
    caml_modify, caml_register_generational_global_root, caml_remove_generational_global_root,
    field, int_val, val_int, Value,
};
use stag_ast::{typed, Str};

/// The OCaml `Stamp_counter.t` (`{ mutable next : int }`) the typechecker
/// mints idents with, rooted, so idents minted in Rust are distinct from its
/// even across calls into OCaml.
pub struct Stamps {
    counter: Box<Value>,
}

impl Stamps {
    /// # Safety
    ///
    /// `counter` must be a `Stamp_counter.t`, and the `Stamps` dropped
    /// before returning to OCaml.
    pub unsafe fn new(counter: Value) -> Stamps {
        let mut counter = Box::new(counter);
        caml_register_generational_global_root(&mut *counter);
        Stamps { counter }
    }

    pub fn next(&mut self) -> i64 {
        unsafe {
            let next = field(*self.counter, 0);
            let stamp = int_val(*next) as i64;
            caml_modify(next, val_int(stamp as isize + 1));
            stamp
        }
    }

    pub fn fresh(&mut self, name: Str) -> typed::Ident {
        typed::Ident {
            name,
            stamp: self.next(),
        }
    }
}

impl Drop for Stamps {
    fn drop(&mut self) {
        unsafe { caml_remove_generational_global_root(&mut *self.counter) }
    }
}
