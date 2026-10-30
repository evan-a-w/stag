#[allow(clippy::module_inception)]
mod interp;

pub mod bytecode;
pub mod entry;
pub mod interp_types;
pub mod stamps;

pub use interp::Interp;
pub use stamps::Stamps;
