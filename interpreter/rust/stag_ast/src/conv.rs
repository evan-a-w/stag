use std::cell::Cell;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::panic;
use std::sync::Once;

use crate::arena::{Pending, Ref, Store};
use crate::raw::{self, Value};
use crate::{Arena, ArenaBuilder};

/// A Rust type converted from OCaml values of its OCaml type.
pub trait FromOcaml: Sized {
    /// # Safety
    ///
    /// As [`convert`], which converts with it.
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self;
}

/// The types whose values live in an [`Arena`], each OCaml value once.
pub trait Stored: FromOcaml + 'static {
    fn pending(builder: &mut ArenaBuilder) -> &mut Pending<Self>;
    fn store(arena: &Arena) -> &Store<Self>;
    fn store_mut(arena: &mut Arena) -> &mut Store<Self>;
}

impl<T: Stored> std::ops::Index<Ref<T>> for Arena {
    type Output = T;
    fn index(&self, id: Ref<T>) -> &T {
        &T::store(self)[id]
    }
}

impl<T: Stored> std::ops::IndexMut<Ref<T>> for Arena {
    fn index_mut(&mut self, id: Ref<T>) -> &mut T {
        &mut T::store_mut(self)[id]
    }
}

/// Why a conversion failed: a value that is not of its type, where it could
/// tell.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error(pub String);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for Error {}

thread_local! {
    static CONVERTING: Cell<bool> = const { Cell::new(false) };
}

/// Panics of conversions are returned as [`Error`]s, so are not printed.
fn quiet_panics_of_conversions() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let default = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if !CONVERTING.with(Cell::get) {
                default(info)
            }
        }))
    });
}

/// Converts `v`, a value of the OCaml type of `T`, and the values it
/// refers to, with the [`Arena`] of their stored values.
///
/// # Safety
///
/// `v` must be a value of the OCaml type of `T`: other values are reported
/// as [`Error`]s only where they could not be one (eg. an unexpected tag),
/// and may otherwise crash. No OCaml code may run until it returns, so that
/// the GC does not move values.
pub unsafe fn convert<T: FromOcaml>(v: Value) -> Result<(T, Arena), Error> {
    quiet_panics_of_conversions();
    CONVERTING.with(|converting| converting.set(true));
    let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
        let mut builder = ArenaBuilder::default();
        let x = T::from_ocaml(v, &mut builder);
        (x, builder.finish())
    }));
    CONVERTING.with(|converting| converting.set(false));
    result.map_err(|panic| {
        let message = panic
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| panic.downcast_ref::<&str>().copied())
            .unwrap_or("conversion panicked");
        Error(message.to_owned())
    })
}

impl<T: Stored> FromOcaml for Ref<T> {
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
        let (id, new) = T::pending(cx).reserve(raw::address(v));
        if new {
            let x = T::from_ocaml(v, cx);
            T::pending(cx).fill(id, x);
        }
        id
    }
}

impl FromOcaml for i64 {
    unsafe fn from_ocaml(v: Value, _: &mut ArenaBuilder) -> Self {
        raw::long(v)
    }
}

impl FromOcaml for bool {
    unsafe fn from_ocaml(v: Value, _: &mut ArenaBuilder) -> Self {
        raw::long(v) != 0
    }
}

/// OCaml's `char`.
impl FromOcaml for u8 {
    unsafe fn from_ocaml(v: Value, _: &mut ArenaBuilder) -> Self {
        raw::long(v) as u8
    }
}

impl FromOcaml for () {
    unsafe fn from_ocaml(_: Value, _: &mut ArenaBuilder) -> Self {}
}

/// OCaml strings are bytes: those that are not UTF-8 are converted lossily.
impl FromOcaml for String {
    unsafe fn from_ocaml(v: Value, _: &mut ArenaBuilder) -> Self {
        String::from_utf8_lossy(raw::bytes(v)).into_owned()
    }
}

impl<T: FromOcaml> FromOcaml for Box<T> {
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
        Box::new(T::from_ocaml(v, cx))
    }
}

/// OCaml's `list`.
impl<T: FromOcaml> FromOcaml for Vec<T> {
    unsafe fn from_ocaml(mut v: Value, cx: &mut ArenaBuilder) -> Self {
        let mut xs = Vec::new();
        while !raw::is_long(v) {
            if raw::tag(v) != 0 || raw::size(v) != 2 {
                raw::bad("list", v)
            }
            xs.push(T::from_ocaml(raw::field(v, 0), cx));
            v = raw::field(v, 1);
        }
        xs
    }
}

/// OCaml's `array`, not of floats (whose blocks are unboxed).
impl<T: FromOcaml> FromOcaml for Box<[T]> {
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
        (0..raw::size(v))
            .map(|i| T::from_ocaml(raw::field(v, i), cx))
            .collect()
    }
}

impl<T: FromOcaml> FromOcaml for Option<T> {
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
        if raw::is_long(v) {
            None
        } else {
            Some(T::from_ocaml(raw::field(v, 0), cx))
        }
    }
}

macro_rules! tuple {
    ($($t:ident $i:tt),*) => {
        impl<$($t: FromOcaml),*> FromOcaml for ($($t,)*) {
            unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
                ($($t::from_ocaml(raw::field(v, $i), cx),)*)
            }
        }
    };
}

tuple!(A 0, B 1);
tuple!(A 0, B 1, C 2);
tuple!(A 0, B 1, C 2, D 3);
tuple!(A 0, B 1, C 2, D 3, E 4);
tuple!(A 0, B 1, C 2, D 3, E 4, F 5);

/// OCaml's `Nothing.t`, which has no values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Nothing {}

impl FromOcaml for Nothing {
    unsafe fn from_ocaml(v: Value, _: &mut ArenaBuilder) -> Self {
        raw::bad("Nothing.t", v)
    }
}

/// Hashes the elements of an unordered collection independently of their
/// order.
fn hash_unordered<H: Hasher>(len: usize, xs: impl Iterator<Item = u64>, state: &mut H) {
    len.hash(state);
    xs.fold(0u64, u64::wrapping_add).hash(state);
}

fn hash_one(x: &impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    x.hash(&mut hasher);
    hasher.finish()
}

/// The [`fmt::Debug`] of `xs`, sorted by it so that it is deterministic.
fn sorted_debug<X: fmt::Debug>(xs: impl Iterator<Item = X>) -> Vec<X> {
    let mut xs: Vec<(String, X)> = xs.map(|x| (format!("{x:?}"), x)).collect();
    xs.sort_by(|(a, _), (b, _)| a.cmp(b));
    xs.into_iter().map(|(_, x)| x).collect()
}

/// A Core `Map.t`. Its [`fmt::Debug`] is sorted, and it hashes regardless
/// of order.
#[derive(Clone)]
pub struct Map<K, V>(pub HashMap<K, V>);

impl<K, V> Default for Map<K, V> {
    fn default() -> Self {
        Map(HashMap::new())
    }
}

impl<K, V> Deref for Map<K, V> {
    type Target = HashMap<K, V>;
    fn deref(&self) -> &HashMap<K, V> {
        &self.0
    }
}

impl<K, V> DerefMut for Map<K, V> {
    fn deref_mut(&mut self) -> &mut HashMap<K, V> {
        &mut self.0
    }
}

impl<K: Eq + Hash, V: PartialEq> PartialEq for Map<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<K: Eq + Hash, V: Eq> Eq for Map<K, V> {}

impl<K: Hash, V: Hash> Hash for Map<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_unordered(self.0.len(), self.0.iter().map(|kv| hash_one(&kv)), state)
    }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for Map<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(sorted_debug(self.0.iter())).finish()
    }
}

/// Base's `Map.Tree0.t`: `Empty | Leaf { key; data } | Node { left; key;
/// data; right; height }`.
unsafe fn map_tree<K: FromOcaml + Eq + Hash, V: FromOcaml>(
    v: Value,
    cx: &mut ArenaBuilder,
    out: &mut HashMap<K, V>,
) {
    if raw::is_long(v) {
        return;
    }
    match raw::tag(v) {
        0 => {
            out.insert(
                K::from_ocaml(raw::field(v, 0), cx),
                V::from_ocaml(raw::field(v, 1), cx),
            );
        }
        1 => {
            map_tree(raw::field(v, 0), cx, out);
            out.insert(
                K::from_ocaml(raw::field(v, 1), cx),
                V::from_ocaml(raw::field(v, 2), cx),
            );
            map_tree(raw::field(v, 3), cx, out);
        }
        _ => raw::bad("Map.t", v),
    }
}

/// Base's `Map.t`: `{ comparator; tree; length }`.
impl<K: FromOcaml + Eq + Hash, V: FromOcaml> FromOcaml for Map<K, V> {
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
        let mut out = HashMap::with_capacity(raw::long(raw::field(v, 2)) as usize);
        map_tree(raw::field(v, 1), cx, &mut out);
        Map(out)
    }
}

/// A Core `Set.t`. Its [`fmt::Debug`] is sorted, and it hashes regardless
/// of order.
#[derive(Clone)]
pub struct Set<T>(pub HashSet<T>);

impl<T> Default for Set<T> {
    fn default() -> Self {
        Set(HashSet::new())
    }
}

impl<T> Deref for Set<T> {
    type Target = HashSet<T>;
    fn deref(&self) -> &HashSet<T> {
        &self.0
    }
}

impl<T> DerefMut for Set<T> {
    fn deref_mut(&mut self) -> &mut HashSet<T> {
        &mut self.0
    }
}

impl<T: Eq + Hash> PartialEq for Set<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq + Hash> Eq for Set<T> {}

impl<T: Hash> Hash for Set<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_unordered(self.0.len(), self.0.iter().map(hash_one), state)
    }
}

impl<T: fmt::Debug> fmt::Debug for Set<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(sorted_debug(self.0.iter())).finish()
    }
}

/// Base's `Set.Tree0.t`: `Empty | Leaf { elt } | Node { left; elt; right;
/// height; size }`.
unsafe fn set_tree<T: FromOcaml + Eq + Hash>(
    v: Value,
    cx: &mut ArenaBuilder,
    out: &mut HashSet<T>,
) {
    if raw::is_long(v) {
        return;
    }
    match raw::tag(v) {
        0 => {
            out.insert(T::from_ocaml(raw::field(v, 0), cx));
        }
        1 => {
            set_tree(raw::field(v, 0), cx, out);
            out.insert(T::from_ocaml(raw::field(v, 1), cx));
            set_tree(raw::field(v, 2), cx, out);
        }
        _ => raw::bad("Set.t", v),
    }
}

/// Base's `Set.t`: `{ comparator; tree }`.
impl<T: FromOcaml + Eq + Hash> FromOcaml for Set<T> {
    unsafe fn from_ocaml(v: Value, cx: &mut ArenaBuilder) -> Self {
        let mut out = HashSet::new();
        set_tree(raw::field(v, 1), cx, &mut out);
        Set(out)
    }
}
