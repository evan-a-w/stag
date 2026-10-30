//! The Rust code linked into OCaml, as the static library of the OCaml
//! library `stag_rust` (`interpreter/stag_rust`), which declares its
//! externals.

pub mod debug;
pub mod interp;
pub mod stamps;

/// Converts OCaml strings to Rust ones and back, for externals.
pub(crate) mod ocaml_string {
    use ocaml_sys::Value;

    /// # Safety
    ///
    /// Allocates on the OCaml heap: no OCaml value may be held (unrooted)
    /// across it.
    pub unsafe fn of_str(s: &str) -> Value {
        ocaml_sys::caml_alloc_initialized_string(s.len(), s.as_ptr().cast())
    }
}
