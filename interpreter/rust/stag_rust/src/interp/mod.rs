#[allow(clippy::module_inception)]
mod interp;

pub mod entry;
pub mod stamps;

pub use interp::Interp;
pub use stamps::Stamps;
