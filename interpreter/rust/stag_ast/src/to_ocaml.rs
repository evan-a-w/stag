//! Building OCaml values of Rust ones: the inverse of [`crate::convert`].
//!
//! Every allocation may run the GC, which moves young values, so no OCaml
//! value is held in a Rust variable across one. Blocks are allocated before
//! their fields, kept in the root stack of the [`Builder`] (an OCaml array,
//! the one generational global root of a build) while their fields are built,
//! and each field is written as soon as it is built, so there are only as many
//! roots as the value is deep. The blocks of the [`Arena`]'s values, by
//! [`Ref`], are kept in arrays at the bottom of the root stack.

use std::collections::HashMap;
use std::ffi::CString;

use ocaml_sys::{
    caml_alloc, caml_alloc_string, caml_callback_exn, caml_modify, caml_named_value,
    caml_register_generational_global_root, caml_remove_generational_global_root,
    extract_exception, is_exception_result, val_int, Value,
};

use crate::conv::{catching, Map, Nothing, Set, Stored};
use crate::{Arena, Error, Ref};

/// A Rust type of which OCaml values of its OCaml type are built.
pub trait ToOcaml {
    /// The OCaml value of `self`, which no allocation may happen before it is
    /// stored (eg. in a block of the builder's roots, by [`Builder::set`]).
    ///
    /// # Safety
    ///
    /// As [`to_ocaml`], which builds with it.
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value;
}

/// The blocks, or immediates, of the values of declarations, which are built
/// in two steps: [`Construct::shell`], then [`Construct::fill`], so that
/// cycles through stored values can refer to their shells.
pub trait Construct {
    /// The name of the OCaml callback rebuilding values whose `[@rust.skip]`
    /// fields are `()`: see `Ppx_rust_runtime.register_rebuild`.
    const REBUILD: Option<&'static str>;

    /// A block of the right tag and size whose fields are `()`, or the
    /// immediate of a constant constructor.
    ///
    /// # Safety
    ///
    /// Allocates.
    unsafe fn shell(&self, cx: &mut Builder) -> Value;

    /// Builds the fields of the shell at `slot` of the roots.
    ///
    /// # Safety
    ///
    /// As [`to_ocaml`].
    unsafe fn fill(&self, slot: Slot, cx: &mut Builder);
}

/// A stored type, built once per [`Ref`].
pub trait Indexed: Stored + Construct {
    /// The index of its store among the [`Arena`]'s.
    const STORE: usize;
}

/// A type of map keys and set elements, whose maps and sets are built by the
/// OCaml callbacks `[@@deriving rust]` registers with its comparator.
pub trait OcamlKey {
    /// `Ppx_rust_runtime.Callback_name.map` of the type.
    const MAP: &'static str;
    /// `Ppx_rust_runtime.Callback_name.set` of the type.
    const SET: &'static str;
}

/// A position in the roots of a [`Builder`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Slot(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Unbuilt,
    Building,
    Built,
}

/// Builds OCaml values of the values of an [`Arena`].
pub struct Builder<'a> {
    arena: &'a Arena,
    /// An OCaml array: the blocks of the arena's stores, by [`Ref`] (one array
    /// per store), then a stack of blocks being filled.
    roots: Box<Value>,
    len: usize,
    states: Vec<Vec<State>>,
    callbacks: HashMap<&'static str, *const Value>,
    /// Whether to collect the minor heap before every allocation (and compact
    /// sometimes), so that values Rust holds unrooted move, then fill the
    /// minor heap with garbage, so that they are overwritten.
    gc_stress: bool,
    /// The size of the minor heap, in words, if `gc_stress`.
    minor_heap_words: usize,
    allocations: usize,
}

impl Builder<'_> {
    /// # Safety
    ///
    /// Allocates.
    unsafe fn alloc(&mut self, size: usize, tag: u8) -> Value {
        self.stress();
        caml_alloc(size, tag)
    }

    unsafe fn stress(&mut self) {
        if self.gc_stress {
            self.allocations += 1;
            if self.allocations.is_multiple_of(4096) {
                caml_gc_compaction(val_int(0));
            } else {
                caml_gc_minor(val_int(0));
            }
            // Without this, the next minor collection would find values held
            // unrooted, but stored since, by their forwarding pointers.
            for _ in 0..(self.minor_heap_words / POISON_WORDS).saturating_sub(2) {
                let poison = caml_alloc_string(POISON_BYTES);
                std::ptr::write_bytes(poison as *mut u8, 0xAB, POISON_BYTES);
            }
        }
    }

    fn roots(&self) -> Value {
        *self.roots
    }

    fn capacity(&self) -> usize {
        unsafe { ocaml_sys::wosize_val(self.roots()) }
    }

    /// Pushes `v` on the root stack, while it is filled.
    ///
    /// # Safety
    ///
    /// May allocate, keeping `v` alive.
    pub unsafe fn push(&mut self, v: Value) -> Slot {
        let v = if self.len == self.capacity() {
            self.grow(v)
        } else {
            v
        };
        let slot = Slot(self.len);
        self.len += 1;
        self.set_root(slot, v);
        slot
    }

    /// Doubles the root stack, giving `v`, kept alive.
    #[cold]
    unsafe fn grow(&mut self, v: Value) -> Value {
        let mut v = Box::new(v);
        caml_register_generational_global_root(&mut *v);
        let old = self.capacity();
        let bigger = self.alloc(old * 2, 0);
        for i in 0..old {
            caml_modify(
                ocaml_sys::field(bigger, i),
                *ocaml_sys::field(self.roots(), i),
            );
        }
        ocaml_sys::caml_modify_generational_global_root(&mut *self.roots, bigger);
        caml_remove_generational_global_root(&mut *v);
        *v
    }

    /// Pops `slot`, the top of the root stack, giving its value.
    pub fn pop(&mut self, slot: Slot) -> Value {
        assert_eq!(slot.0 + 1, self.len, "popping a slot not on top");
        let v = self.get(slot);
        unsafe { self.set_root(slot, val_int(0)) };
        self.len -= 1;
        v
    }

    pub fn get(&self, slot: Slot) -> Value {
        unsafe { *ocaml_sys::field(self.roots(), slot.0) }
    }

    unsafe fn set_root(&mut self, slot: Slot, v: Value) {
        caml_modify(ocaml_sys::field(self.roots(), slot.0), v)
    }

    /// Sets field `i` of the block at `slot` to `x`.
    ///
    /// # Safety
    ///
    /// The block must have the field.
    pub unsafe fn set(&mut self, slot: Slot, i: usize, x: Value) {
        caml_modify(ocaml_sys::field(self.get(slot), i), x)
    }

    /// A block of `size` fields, all `()`.
    ///
    /// # Safety
    ///
    /// Allocates.
    pub unsafe fn block(&mut self, size: usize, tag: u8) -> Value {
        self.alloc(size, tag)
    }

    /// Builds `x`'s value, with [`Construct::REBUILD`] if any.
    ///
    /// # Safety
    ///
    /// As [`to_ocaml`].
    pub unsafe fn construct<T: Construct>(&mut self, x: &T) -> Value {
        let v = x.shell(self);
        let slot = self.push(v);
        x.fill(slot, self);
        let v = self.pop(slot);
        match T::REBUILD {
            None => v,
            Some(name) => self.call(name, v),
        }
    }

    /// Calls the OCaml callback registered as `name` with `arg`.
    unsafe fn call(&mut self, name: &'static str, arg: Value) -> Value {
        let f = *self.callbacks.entry(name).or_insert_with(|| {
            let c_name = CString::new(name).unwrap();
            caml_named_value(c_name.as_ptr())
        });
        if f.is_null() {
            panic!("no OCaml callback {name} (is its library linked?)")
        }
        let arg = if self.gc_stress {
            let slot = self.push(arg);
            self.stress();
            self.pop(slot)
        } else {
            arg
        };
        let result = caml_callback_exn(*f, arg);
        if is_exception_result(result) {
            panic!(
                "OCaml callback {name} raised {}",
                exception_name(extract_exception(result))
            )
        }
        result
    }

    unsafe fn stored<T: Indexed>(&mut self, id: Ref<T>) -> Value {
        let store = Slot(T::STORE);
        let i = id.index();
        match self.states[T::STORE][i] {
            State::Built => return *ocaml_sys::field(self.get(store), i),
            State::Building if T::REBUILD.is_some() => {
                panic!("a cycle through a value rebuilt by an OCaml callback")
            }
            State::Building => return *ocaml_sys::field(self.get(store), i),
            State::Unbuilt => {}
        }
        let arena = self.arena;
        let x = &arena[id];
        self.states[T::STORE][i] = State::Building;
        let v = x.shell(self);
        self.set(store, i, v);
        let slot = self.push(v);
        x.fill(slot, self);
        let mut v = self.pop(slot);
        if let Some(name) = T::REBUILD {
            v = self.call(name, v);
            self.set(store, i, v);
        }
        self.states[T::STORE][i] = State::Built;
        v
    }

    /// Builds a list of the values of `xs`.
    unsafe fn list<X>(
        &mut self,
        xs: impl ExactSizeIterator<Item = X>,
        mut f: impl FnMut(&mut Self, X) -> Value,
    ) -> Value {
        if xs.len() == 0 {
            return val_int(0);
        }
        let head = self.block(2, 0);
        let head = self.push(head);
        let last = self.push(self.get(head));
        for (i, x) in xs.enumerate() {
            if i > 0 {
                let cell = self.block(2, 0);
                self.set(last, 1, cell);
                self.set_root(last, cell);
            }
            let x = f(self, x);
            self.set(last, 0, x);
        }
        self.pop(last);
        self.pop(head)
    }
}

/// Strings as big as the minor heap allocates, with their headers.
const POISON_BYTES: usize = 255 * 8;
const POISON_WORDS: usize = 257;

extern "C" {
    fn caml_gc_get(v: Value) -> Value;
    fn caml_gc_minor(v: Value) -> Value;
    fn caml_gc_compaction(v: Value) -> Value;
    fn caml_format_exception(v: Value) -> *mut std::ffi::c_char;
    fn free(p: *mut std::ffi::c_void);
}

unsafe fn exception_name(exn: Value) -> String {
    let s = caml_format_exception(exn);
    let name = std::ffi::CStr::from_ptr(s).to_string_lossy().into_owned();
    free(s.cast());
    name
}

/// Builds the OCaml value of `x`, whose [`Ref`]s are into `arena`. Values of
/// the arena are built once each, so OCaml values share them as Rust ones do,
/// and cycles through them are rebuilt. With `gc_stress`, for tests, the GC
/// runs before every allocation, then fills the minor heap with garbage (so
/// set the minor heap small).
///
/// # Safety
///
/// Must be called from OCaml (eg. in an external), holding the runtime lock.
/// The value returned is not rooted: it must be returned or rooted before
/// anything allocates.
pub unsafe fn to_ocaml<T: ToOcaml>(x: &T, arena: &Arena, gc_stress: bool) -> Result<Value, Error> {
    catching(|| {
        let lens = arena.store_lens();
        let minor_heap_words = if gc_stress {
            ocaml_sys::int_val(*ocaml_sys::field(caml_gc_get(val_int(0)), 0)) as usize
        } else {
            0
        };
        let mut root = Box::new(caml_alloc(lens.len() + 64, 0));
        caml_register_generational_global_root(&mut *root);
        let mut cx = RootGuard(Builder {
            arena,
            roots: root,
            len: 0,
            states: lens.iter().map(|&n| vec![State::Unbuilt; n]).collect(),
            callbacks: HashMap::new(),
            gc_stress,
            minor_heap_words,
            allocations: 0,
        });
        let cx = &mut cx.0;
        for len in lens {
            let store = cx.block(len, 0);
            cx.push(store);
        }
        x.to_ocaml(cx)
    })
}

/// Removes the root of the builder, even when building panics.
struct RootGuard<'a>(Builder<'a>);

impl Drop for RootGuard<'_> {
    fn drop(&mut self) {
        unsafe { caml_remove_generational_global_root(&mut *self.0.roots) }
    }
}

impl<T: Indexed> ToOcaml for Ref<T> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        cx.stored(*self)
    }
}

impl ToOcaml for i64 {
    unsafe fn to_ocaml(&self, _: &mut Builder) -> Value {
        val_int(*self as isize)
    }
}

impl ToOcaml for bool {
    unsafe fn to_ocaml(&self, _: &mut Builder) -> Value {
        val_int(*self as isize)
    }
}

impl ToOcaml for u8 {
    unsafe fn to_ocaml(&self, _: &mut Builder) -> Value {
        val_int(*self as isize)
    }
}

impl ToOcaml for () {
    unsafe fn to_ocaml(&self, _: &mut Builder) -> Value {
        val_int(0)
    }
}

impl ToOcaml for String {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        cx.stress();
        let v = caml_alloc_string(self.len());
        std::ptr::copy_nonoverlapping(self.as_ptr(), v as *mut u8, self.len());
        v
    }
}

impl ToOcaml for Nothing {
    unsafe fn to_ocaml(&self, _: &mut Builder) -> Value {
        match *self {}
    }
}

impl<T: ToOcaml + ?Sized> ToOcaml for &T {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        T::to_ocaml(self, cx)
    }
}

impl<T: ToOcaml> ToOcaml for Box<T> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        T::to_ocaml(self, cx)
    }
}

impl<T: ToOcaml> ToOcaml for Vec<T> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        cx.list(self.iter(), |cx, x| x.to_ocaml(cx))
    }
}

impl<T: ToOcaml> ToOcaml for Box<[T]> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        let v = cx.block(self.len(), 0);
        let slot = cx.push(v);
        for (i, x) in self.iter().enumerate() {
            let x = x.to_ocaml(cx);
            cx.set(slot, i, x);
        }
        cx.pop(slot)
    }
}

impl<T: ToOcaml> ToOcaml for Option<T> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        match self {
            None => val_int(0),
            Some(x) => {
                let v = cx.block(1, 0);
                let slot = cx.push(v);
                let x = x.to_ocaml(cx);
                cx.set(slot, 0, x);
                cx.pop(slot)
            }
        }
    }
}

macro_rules! tuple {
    ($n:expr; $($t:ident $i:tt),*) => {
        impl<$($t: ToOcaml),*> ToOcaml for ($($t,)*) {
            unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
                let v = cx.block($n, 0);
                let slot = cx.push(v);
                $(
                    let x = self.$i.to_ocaml(cx);
                    cx.set(slot, $i, x);
                )*
                cx.pop(slot)
            }
        }
    };
}

tuple!(2; A 0, B 1);
tuple!(3; A 0, B 1, C 2);
tuple!(4; A 0, B 1, C 2, D 3);
tuple!(5; A 0, B 1, C 2, D 3, E 4);
tuple!(6; A 0, B 1, C 2, D 3, E 4, F 5);

/// Built by the OCaml callback of its key type, of an association list.
impl<K: ToOcaml + OcamlKey, V: ToOcaml> ToOcaml for Map<K, V> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        let alist = cx.list(self.iter(), |cx, (k, v)| (k, v).to_ocaml(cx));
        cx.call(K::MAP, alist)
    }
}

/// Built by the OCaml callback of its element type, of a list.
impl<T: ToOcaml + OcamlKey> ToOcaml for Set<T> {
    unsafe fn to_ocaml(&self, cx: &mut Builder) -> Value {
        let elts = cx.list(self.iter(), |cx, x| x.to_ocaml(cx));
        cx.call(T::SET, elts)
    }
}

macro_rules! key {
    ($t:ty, $name:literal) => {
        impl OcamlKey for $t {
            const MAP: &'static str = concat!("ppx_rust.map.", $name);
            const SET: &'static str = concat!("ppx_rust.set.", $name);
        }
    };
}

key!(i64, "int");
key!(String, "string");
key!(u8, "char");
key!(bool, "bool");
