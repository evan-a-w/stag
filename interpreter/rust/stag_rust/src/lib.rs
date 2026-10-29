//! The Rust code linked into OCaml, as the static library of the OCaml
//! library `stag_rust` (`interpreter/stag_rust`), which declares its
//! externals.

pub mod debug;

/// Converts OCaml strings to Rust ones and back, for externals.
pub(crate) mod ocaml_string {
    use ocaml_sys::Value;

    /// # Safety
    ///
    /// Allocates on the OCaml heap: no OCaml value may be held (unrooted)
    /// across it.
    pub unsafe fn of_str(s: &str) -> Value {
        let v = ocaml_sys::caml_alloc_string(s.len());
        std::ptr::copy_nonoverlapping(s.as_ptr(), v as *mut u8, s.len());
        v
    }
}
