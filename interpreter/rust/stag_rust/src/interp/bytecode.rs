use std::collections::HashMap;

use stag_ast::{syntax::Lit, typed::Ident};

pub type StackOffset = usize;

pub struct VarInfo {
    pub size: StackOffset,
    pub stack_offset: StackOffset,
}

pub enum Instruction {
    Move(StackOffset, StackOffset), // dest <- src
    Set(StackOffset, Lit),          // dest <- const
    GetGlobal(StackOffset, usize),
    Call(StackOffset, StackOffset), // second stack offset is the beginning of the args
}

pub struct CompiledToplevel {
    instructions: Vec<Instruction>,
    var_info: HashMap<Ident, VarInfo>,
    stack_frame_size: StackOffset,
}
