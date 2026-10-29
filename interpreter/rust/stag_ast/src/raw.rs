//! Reading OCaml values, which must stay where they are while they are read:
//! nothing may run the OCaml GC meanwhile.
//!
//! # Safety
//!
//! The `unsafe` functions take values of the OCaml types they read (eg. a
//! block for [`tag`]), live and unmoved while they run.

#![allow(clippy::missing_safety_doc)]

pub use ocaml_sys::Value;

#[inline]
pub fn is_long(v: Value) -> bool {
    ocaml_sys::is_long(v)
}

#[inline]
pub unsafe fn long(v: Value) -> i64 {
    ocaml_sys::int_val(v) as i64
}

#[inline]
pub unsafe fn tag(v: Value) -> u8 {
    ocaml_sys::tag_val(v)
}

#[inline]
pub unsafe fn size(v: Value) -> usize {
    ocaml_sys::wosize_val(v) as usize
}

/// Field `i` of `v`, checked to be a block with it, so that values of the
/// wrong type are more often [`bad`] rather than read out of bounds.
#[inline]
pub unsafe fn field(v: Value, i: usize) -> Value {
    if is_long(v) || i >= size(v) {
        missing_field(v, i)
    }
    *ocaml_sys::field(v, i)
}

#[cold]
unsafe fn missing_field(v: Value, i: usize) -> ! {
    if is_long(v) {
        panic!("expected a block with field {i}, got immediate {}", long(v))
    } else {
        panic!(
            "expected a block with field {i}, got one of size {}",
            size(v)
        )
    }
}

/// The bytes of a string: its block is padded so that its last byte is the
/// number of padding bytes before it.
pub unsafe fn bytes<'a>(v: Value) -> &'a [u8] {
    if is_long(v) || tag(v) != ocaml_sys::STRING || size(v) == 0 {
        bad("string", v)
    }
    let words = size(v) * std::mem::size_of::<Value>();
    let ptr = v as *const u8;
    let len = words - 1 - *ptr.add(words - 1) as usize;
    std::slice::from_raw_parts(ptr, len)
}

/// The OCaml block address of `v`, identifying it during a conversion.
#[inline]
pub fn address(v: Value) -> usize {
    v as usize
}

#[cold]
pub unsafe fn bad(ty: &str, v: Value) -> ! {
    if is_long(v) {
        panic!("{ty}: unexpected immediate {}", long(v))
    } else {
        panic!(
            "{ty}: unexpected block of tag {} and size {}",
            tag(v),
            size(v)
        )
    }
}
