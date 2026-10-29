//! The support code of `stag_ast` with the Rust generated from the example
//! declarations of `interpreter/gen/example`, so that rustc checks what the
//! generator makes of the cases stag's own types do not have.

#[path = "../../stag_ast/src/arena.rs"]
mod arena;
#[path = "../../stag_ast/src/conv.rs"]
mod conv;
#[path = "../../stag_ast/src/ostring.rs"]
mod ostring;
#[path = "../../stag_ast/src/raw.rs"]
pub mod raw;
#[path = "../../stag_ast/src/to_ocaml.rs"]
mod to_ocaml;

pub use arena::{Pending, Ref, Store};
pub use conv::{convert, convert_into, Error, FromOcaml, Map, Nothing, Set, Stored};
pub use ostring::Str;
pub use to_ocaml::{to_ocaml, Builder, Construct, GcStress, Indexed, OcamlKey, Slot, ToOcaml};

#[allow(clippy::all)]
mod generated {
    include!("generated.rs");
}

pub use generated::*;

#[cfg(test)]
#[path = "../../stag_ast/src/test_heap.rs"]
mod test_heap;
#[cfg(test)]
mod tests;
