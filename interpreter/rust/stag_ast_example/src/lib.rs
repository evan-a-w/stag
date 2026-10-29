//! The support code of `stag_ast` with the Rust generated from the example
//! declarations of `interpreter/gen/example`, so that rustc checks what the
//! generator makes of the cases stag's own types do not have.

#[path = "../../stag_ast/src/arena.rs"]
mod arena;
#[path = "../../stag_ast/src/conv.rs"]
mod conv;
#[path = "../../stag_ast/src/raw.rs"]
pub mod raw;

pub use arena::{Pending, Ref, Store};
pub use conv::{convert, Error, FromOcaml, Map, Nothing, Set, Stored};

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
