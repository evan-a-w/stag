//! Rust versions of stag's OCaml types (see `generated.rs`, made by
//! `interpreter/gen` from the `[@@deriving rust]` declarations), [`convert`]ed
//! once from OCaml values, and built back into OCaml values by [`to_ocaml`].
//!
//! Values of mutable records and of `[@@rust.shared]` declarations live in
//! the [`Arena`] of the conversion, one per OCaml value, and are referred to
//! by [`Ref`]s, so sharing and cycles are kept. Everything else is owned.

mod arena;
mod conv;
mod ostring;
mod pool;
pub mod raw;
mod to_ocaml;

pub use arena::{Pending, Ref, Store};
pub use conv::{convert, convert_into, Error, FromOcaml, Map, Nothing, Set, Stored};
pub use ostring::Str;
pub use pool::Pool;
pub use to_ocaml::{to_ocaml, Builder, Construct, GcStress, Indexed, OcamlKey, Slot, ToOcaml};

#[allow(clippy::all)]
mod generated {
    include!("generated.rs");
}

pub use generated::*;

#[cfg(test)]
mod test_heap;
#[cfg(test)]
mod tests;
