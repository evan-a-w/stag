use std::hash::Hash;
use std::{collections::HashMap, hash::Hasher};

use stag_ast::{syntax::Lit, type_::Ty, typed::Ident};

pub type StackOffset = usize;

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

pub enum Instruction {
    Move(StackOffset, StackOffset), // dest <- src
    SetConst(StackOffset, Lit),     // dest <- const
    GetGlobal(StackOffset, usize),
    Call(StackOffset, StackOffset), // second stack offset is the beginning of the args
    PolyGet(StackOffset, PolyId),
    Deref(StackOffset, StackOffset, StackOffset), // (dst, src, offset): dst <- *(src + offset)
    CloseUpValuesAfter(StackOffset), // upvalues sorted decreasing by stack slot. Pop while >= this
}

pub struct CompiledToplevel {
    instructions: Vec<Instruction>,
    var_info: HashMap<Ident, VarInfo>,
    stack_frame_size: StackOffset,
}

// #[derive(Default, Clone)]
// pub struct PolyRecordVtable {
//     size: StackOffset,
//     offsets: Vec<(PolyId, StackOffset)>,
//     types: HashMap<PolyId, Ty>,
//     mem_next: HashMap<(PolyId, Ty), *mut PolyRecordVtable>,
// }

// impl PolyRecordVtable {
//     pub fn extend(&mut self, poly_id: PolyId, ty: Ty) -> *mut Self {
//         self.mem_next.entry((poly_id, ty)).or_insert_with(|| {
//             let res = Self {
//                 offsets: self.offsets.clone(),
//                 types: self.types.clone(),
//                 mem_next: HashMap::new(),
//             };

//             if res.types.contains_key(&poly_id) {
//                 res.types.remove(&poly_id);
//                 if let Ok(idx) = res.offsets.binary_search_by_key(&poly_id, |(a, _)| *a) {
//                     res.offsets.remove(idx);
//                 }
//             }

//             res.types.insert(poly_id, ty);
//             match res.offsets.binary_search_by_key(&poly_id, |(a, _)| *a) {
//                 Ok(_) => assert!(false, "dup id somehow"),
//                 Err(idx) => res.offsets.insert(idx, (poly_id, ty)),
//             }
//             Box::into_raw(Box::new(res))
//         })
//     }
// }
