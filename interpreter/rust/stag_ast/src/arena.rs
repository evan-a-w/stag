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

/// The values of one type in an arena, by [`Ref`].
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Store<T>(Vec<T>);

impl<T> Default for Store<T> {
    fn default() -> Self {
        Store(Vec::new())
    }
}

impl<T> Store<T> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn push(&mut self, x: T) -> Ref<T> {
        self.0.push(x);
        Ref::new((self.0.len() - 1) as u32)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Ref<T>, &T)> {
        self.0
            .iter()
            .enumerate()
            .map(|(i, x)| (Ref::new(i as u32), x))
    }
}

impl<T> std::ops::Index<Ref<T>> for Store<T> {
    type Output = T;
    fn index(&self, id: Ref<T>) -> &T {
        &self.0[id.index()]
    }
}

impl<T> std::ops::IndexMut<Ref<T>> for Store<T> {
    fn index_mut(&mut self, id: Ref<T>) -> &mut T {
        &mut self.0[id.index()]
    }
}

impl<T: fmt::Debug> fmt::Debug for Store<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

/// Values being converted into a [`Store`], after its own: an id is
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
            offset: store.len() as u32,
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
            store.len(),
            self.offset as usize,
            "arena changed while converting into it"
        );
        store.0.extend(
            self.slots
                .into_iter()
                .map(|x| x.expect("unfilled arena slot")),
        );
    }
}
