use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

/// The index of a `T` in an [`Arena`](crate::Arena).
pub struct Ref<T>(u32, PhantomData<fn() -> T>);

impl<T> Ref<T> {
    pub fn new(index: u32) -> Self {
        Ref(index, PhantomData)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Ref<T> {}

impl<T> PartialEq for Ref<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Ref<T> {}

impl<T> PartialOrd for Ref<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Ref<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> std::hash::Hash for Ref<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum Slot<T> {
    Full(T),
    Free { next: Option<u32> },
}

/// Values of one type, by [`Ref`]. Removed values' slots are reused by later
/// inserts, most recently removed first. A [`Ref`] to a removed value must
/// not be used: indexing one panics until its slot is reused, then gives the
/// new value.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Store<T> {
    slots: Vec<Slot<T>>,
    free: Option<u32>,
    len: usize,
}

impl<T> Default for Store<T> {
    fn default() -> Self {
        Store {
            slots: Vec::new(),
            free: None,
            len: 0,
        }
    }
}

impl<T> Store<T> {
    /// The number of values.
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// One more than the greatest index of a [`Ref`] it has handed out.
    pub fn slot_count(&self) -> usize {
        self.slots.len()
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

impl<T> std::ops::Index<Ref<T>> for Store<T> {
    type Output = T;
    fn index(&self, r: Ref<T>) -> &T {
        self.get(r).expect("removed value in a store")
    }
}

impl<T> std::ops::IndexMut<Ref<T>> for Store<T> {
    fn index_mut(&mut self, r: Ref<T>) -> &mut T {
        self.get_mut(r).expect("removed value in a store")
    }
}

impl<T: fmt::Debug> fmt::Debug for Store<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

/// Values being converted into a [`Store`], after its slots (so not into
/// free ones): an id is
/// reserved for an OCaml value before it is converted, so that cycles
/// through it get the same id.
pub struct Pending<T> {
    offset: u32,
    slots: Vec<Option<T>>,
    ids: HashMap<usize, u32>,
}

impl<T> Pending<T> {
    pub fn after(store: &Store<T>) -> Self {
        Pending {
            offset: store.slot_count() as u32,
            slots: Vec::new(),
            ids: HashMap::new(),
        }
    }

    /// The id of the value at `address`, and whether it is new, so needs
    /// [`Pending::fill`]ing.
    pub fn reserve(&mut self, address: usize) -> (Ref<T>, bool) {
        if let Some(&i) = self.ids.get(&address) {
            return (Ref::new(i), false);
        }
        let i = self.offset + self.slots.len() as u32;
        self.slots.push(None);
        self.ids.insert(address, i);
        (Ref::new(i), true)
    }

    pub fn fill(&mut self, id: Ref<T>, x: T) {
        self.slots[id.index() - self.offset as usize] = Some(x);
    }

    pub fn finish_into(self, store: &mut Store<T>) {
        assert_eq!(
            store.slot_count(),
            self.offset as usize,
            "arena changed while converting into it"
        );
        store.len += self.slots.len();
        store.slots.extend(
            self.slots
                .into_iter()
                .map(|x| Slot::Full(x.expect("unfilled arena slot"))),
        );
    }
}
