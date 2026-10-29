//! OCaml values built by hand, outside the OCaml heap, for tests.
#![allow(dead_code)]

use crate::raw::Value;

/// Blocks, kept alive by the heap until it is dropped.
#[derive(Default)]
pub struct Heap(Vec<Box<[usize]>>);

impl Heap {
    pub fn block(&mut self, tag: u8, fields: &[Value]) -> Value {
        let mut words = vec![(fields.len() << 10) | tag as usize];
        words.extend(fields.iter().map(|&f| f as usize));
        let block = words.into_boxed_slice();
        let v = unsafe { block.as_ptr().add(1) } as Value;
        self.0.push(block);
        v
    }

    pub fn string(&mut self, s: &str) -> Value {
        let word = std::mem::size_of::<usize>();
        let words = s.len() / word + 1;
        let mut bytes = vec![0u8; words * word];
        bytes[..s.len()].copy_from_slice(s.as_bytes());
        bytes[words * word - 1] = (words * word - 1 - s.len()) as u8;
        let fields: Vec<Value> = bytes
            .chunks(word)
            .map(|c| usize::from_ne_bytes(c.try_into().unwrap()) as Value)
            .collect();
        self.block(252, &fields)
    }

    pub fn list(&mut self, xs: &[Value]) -> Value {
        xs.iter()
            .rev()
            .fold(int(0), |tl, &hd| self.block(0, &[hd, tl]))
    }

    pub fn array(&mut self, xs: &[Value]) -> Value {
        if xs.is_empty() {
            // OCaml's empty array: the atom of tag 0.
            self.block(0, &[])
        } else {
            self.block(0, xs)
        }
    }

    /// Sets field `i` of `block`, to tie knots.
    pub fn set(&mut self, block: Value, i: usize, x: Value) {
        unsafe { *(block as *mut Value).add(i) = x };
    }
}

pub fn int(n: isize) -> Value {
    (n << 1) | 1
}
