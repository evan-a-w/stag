use std::alloc::{alloc, handle_alloc_error, Layout};
use std::hash::Hash;
use std::ptr::{addr_of_mut, NonNull};
use std::{collections::HashMap, hash::Hasher};

use stag_ast::{syntax::Lit, type_::Ty, typed::Ident};

pub type StackOffset = usize;
pub type UpValueId = usize;
pub type ClosureId = usize;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PolyId(usize);

impl PolyId {
    pub fn new(s: &str) -> Self {
        let mut hasher = std::hash::DefaultHasher::new();
        s.hash(&mut hasher);
        Self(hasher.finish() as usize)
    }
}

pub struct VarInfo {
    pub size: StackOffset,
    pub stack_offset: StackOffset,
    pub ty: Ty,
    pub boxed: bool, // instead of changing types and stuff for boxed locals, we just have this flag
}
