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
}

#[repr(C)]
pub struct UpValue {
    next: Option<NonNull<UpValue>>,
    value: NonNull<u8>,
    size: StackOffset,
    inline_value: [u64; 0], // start of the inline bytes (8-aligned)
}

pub enum FunctionLocal {
    UpValue(UpValueId),
    Local(StackOffset),
}

impl UpValue {
    fn layout(size: usize) -> Layout {
        Layout::from_size_align(size_of::<Self>() + size, align_of::<Self>())
            .unwrap()
            .pad_to_align()
    }

    /// `value: None` points `value` at the inline bytes.
    pub fn new(
        size: StackOffset,
        value: Option<NonNull<u8>>,
        next: Option<NonNull<UpValue>>,
    ) -> NonNull<UpValue> {
        let layout = Self::layout(size as usize);
        unsafe {
            let p = std::alloc::alloc_zeroed(layout) as *mut UpValue;
            if p.is_null() {
                handle_alloc_error(layout);
            }
            let inline = NonNull::new_unchecked((&raw mut (*p).inline_value).cast::<u8>());
            // .write doesn't destruct, so is good for uninitialized stuff
            (&raw mut (*p).next).write(next);
            (&raw mut (*p).value).write(value.unwrap_or(inline));
            (&raw mut (*p).size).write(size);
            NonNull::new_unchecked(p)
        }
    }

    // frees only one node
    pub unsafe fn free(p: NonNull<UpValue>) {
        let size = (*p.as_ptr()).size as usize;
        std::alloc::dealloc(p.as_ptr().cast(), Self::layout(size));
    }
}
