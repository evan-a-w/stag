use std::fmt;

use crate::Ref;

enum Slot<T> {
    Full(T),
    Free { next: Option<u32> },
}

/// A [`Store`](crate::Store) whose values can be removed: removed slots are
/// reused by later inserts, most recently removed first. A [`Ref`] to a
/// removed value must not be used: indexing one panics until its slot is
/// reused, then gives the new value.
pub struct Pool<T> {
    slots: Vec<Slot<T>>,
    free: Option<u32>,
    len: usize,
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Pool {
            slots: Vec::new(),
            free: None,
            len: 0,
        }
    }
}

impl<T> Pool<T> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn insert(&mut self, x: T) -> Ref<T> {
        self.len += 1;
        match self.free {
            Some(i) => {
                let slot = &mut self.slots[i as usize];
                let Slot::Free { next } = *slot else {
                    unreachable!("full slot on the free list")
                };
                self.free = next;
                *slot = Slot::Full(x);
                Ref::new(i)
            }
            None => {
                self.slots.push(Slot::Full(x));
                Ref::new((self.slots.len() - 1) as u32)
            }
        }
    }

    pub fn remove(&mut self, r: Ref<T>) -> Option<T> {
        let slot = self.slots.get_mut(r.index())?;
        if let Slot::Free { .. } = slot {
            return None;
        }
        let Slot::Full(x) = std::mem::replace(slot, Slot::Free { next: self.free }) else {
            unreachable!()
        };
        self.free = Some(r.index() as u32);
        self.len -= 1;
        Some(x)
    }

    pub fn get(&self, r: Ref<T>) -> Option<&T> {
        match self.slots.get(r.index())? {
            Slot::Full(x) => Some(x),
            Slot::Free { .. } => None,
        }
    }

    pub fn get_mut(&mut self, r: Ref<T>) -> Option<&mut T> {
        match self.slots.get_mut(r.index())? {
            Slot::Full(x) => Some(x),
            Slot::Free { .. } => None,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Ref<T>, &T)> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(i, slot)| match slot {
                Slot::Full(x) => Some((Ref::new(i as u32), x)),
                Slot::Free { .. } => None,
            })
    }

    /// Removes the values `keep` is false of, eg. those a GC did not mark.
    pub fn retain(&mut self, mut keep: impl FnMut(Ref<T>, &mut T) -> bool) {
        for i in 0..self.slots.len() {
            let r = Ref::new(i as u32);
            if let Slot::Full(x) = &mut self.slots[i] {
                if !keep(r, x) {
                    self.remove(r);
                }
            }
        }
    }
}

impl<T> std::ops::Index<Ref<T>> for Pool<T> {
    type Output = T;
    fn index(&self, r: Ref<T>) -> &T {
        self.get(r).expect("removed value in a pool")
    }
}

impl<T> std::ops::IndexMut<Ref<T>> for Pool<T> {
    fn index_mut(&mut self, r: Ref<T>) -> &mut T {
        self.get_mut(r).expect("removed value in a pool")
    }
}

impl<T: fmt::Debug> fmt::Debug for Pool<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}
