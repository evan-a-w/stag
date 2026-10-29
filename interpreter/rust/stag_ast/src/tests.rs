use crate::test_heap::{int, Heap};
use crate::{convert, convert_into, type_, Arena, Map, Ref, Set, Str};

#[test]
fn everything_is_send_and_sync() {
    fn check<T: Send + Sync>() {}
    check::<crate::Arena>();
    check::<crate::typed::Program>();
    check::<crate::syntax::Program>();
}

#[test]
fn strings_lists_and_options() {
    let mut heap = Heap::default();
    let strings = ["", "a", "1234567", "12345678", "a longer string"];
    let values: Vec<_> = strings.iter().map(|s| heap.string(s)).collect();
    let list = heap.list(&values);
    let some = heap.block(0, &[list]);
    let (converted, _) = unsafe { convert::<Option<Vec<Str>>>(some) }.unwrap();
    assert_eq!(converted, Some(strings.map(Str::from).to_vec()));
    let (none, _) = unsafe { convert::<Option<Vec<Str>>>(int(0)) }.unwrap();
    assert_eq!(none, None);
}

#[test]
fn maps_and_sets() {
    let mut heap = Heap::default();
    // Node { left = Leaf 1; key 2; data "b"; right = Node { Empty; 3; "c"; Leaf 4 } }
    let [a, b, c, d] = ["a", "b", "c", "d"].map(|s| heap.string(s));
    let leaf1 = heap.block(0, &[int(1), a]);
    let leaf4 = heap.block(0, &[int(4), d]);
    let right = heap.block(1, &[int(0), int(3), c, leaf4, int(2)]);
    let tree = heap.block(1, &[leaf1, int(2), b, right, int(3)]);
    let map = heap.block(0, &[int(0), tree, int(4)]);
    let (map, _) = unsafe { convert::<Map<i64, Str>>(map) }.unwrap();
    assert_eq!(format!("{map:?}"), r#"{1: "a", 2: "b", 3: "c", 4: "d"}"#);
    assert_eq!(map.get(&3).map(Str::as_bytes), Some(&b"c"[..]));
    assert_eq!(map.get(&5), None);

    let leaf = heap.block(0, &[int(5)]);
    let tree = heap.block(1, &[int(0), int(6), leaf, int(2), int(2)]);
    let set = heap.block(0, &[int(0), tree]);
    let (set, _) = unsafe { convert::<Set<i64>>(set) }.unwrap();
    assert_eq!(format!("{set:?}"), "{5, 6}");
    assert!(set.contains(&5) && !set.contains(&4));

    let empty = heap.block(0, &[int(0), int(0)]);
    let (empty, _) = unsafe { convert::<Set<i64>>(empty) }.unwrap();
    assert_eq!(format!("{empty:?}"), "{}");
}

/// `{ id; state; bounds = []; in_bounds_of = []; kept = [] }`, with its state
/// to be set.
fn var(heap: &mut Heap, id: isize) -> crate::raw::Value {
    heap.block(0, &[int(id), int(0), int(0), int(0), int(0)])
}

#[test]
fn cycles_and_sharing_through_vars() {
    let mut heap = Heap::default();
    // var = { id = 7; state = Link (TArrow (TVar var, TVar var)); ... }
    let var = var(&mut heap, 7);
    let tvar = heap.block(6, &[var]);
    let arrow = heap.block(1, &[tvar, tvar]);
    let link = heap.block(1, &[arrow]);
    heap.set(var, 1, link);
    let pair = heap.block(0, &[tvar, tvar]);
    let ((ty, again), arena) =
        unsafe { convert::<(Ref<type_::Ty>, Ref<type_::Ty>)>(pair) }.unwrap();
    assert_eq!(ty, again);
    assert_eq!(
        format!("{ty:?} {:?} {:?}", arena.type_ty, arena.type_var_ty_nothing),
        "#0 {#0: TVar(#0), #1: TArrow(#0, #0)} \
         {#0: Var { id: 7, state: Link(#1), bounds: [], in_bounds_of: [], kept: [] }}"
    );
}

#[test]
fn conversions_into_one_arena_append() {
    let mut heap = Heap::default();
    let first = var(&mut heap, 1);
    let first = heap.block(6, &[first]);
    let second = var(&mut heap, 2);
    let second = heap.block(6, &[second]);
    let mut arena = Arena::default();
    let a = unsafe { convert_into::<Ref<type_::Ty>>(first, &mut arena) }.unwrap();
    let b = unsafe { convert_into::<Ref<type_::Ty>>(second, &mut arena) }.unwrap();
    let again = unsafe { convert_into::<Ref<type_::Ty>>(first, &mut arena) }.unwrap();
    let wrong = unsafe { convert_into::<Ref<type_::Ty>>(int(0), &mut arena) };
    assert_eq!(
        format!(
            "{a:?} {b:?} {again:?} {} {:?} {:?}",
            wrong.is_err(),
            arena.type_ty,
            arena.type_var_ty_nothing
        ),
        "#0 #1 #2 true {#0: TVar(#0), #1: TVar(#1), #2: TVar(#2)} \
         {#0: Var { id: 1, state: Generic, bounds: [], in_bounds_of: [], kept: [] }, \
         #1: Var { id: 2, state: Generic, bounds: [], in_bounds_of: [], kept: [] }, \
         #2: Var { id: 1, state: Generic, bounds: [], in_bounds_of: [], kept: [] }}"
    );
}

/// `TTuple [t; t]` nested `depth` times around a var: as shared as it is in
/// OCaml, rather than `2^depth` types.
#[test]
fn shared_types_stay_shared() {
    let mut heap = Heap::default();
    let var = var(&mut heap, 0);
    heap.set(var, 1, int(0)); // Unbound 0
    let mut ty = heap.block(6, &[var]);
    for _ in 0..64 {
        let list = heap.list(&[ty, ty]);
        ty = heap.block(2, &[list]);
    }
    let (ty, arena) = unsafe { convert::<Ref<type_::Ty>>(ty) }.unwrap();
    assert_eq!(format!("{ty:?}"), "#0");
    assert_eq!(arena.type_ty.len(), 65);
    assert_eq!(format!("{:?}", arena[ty]), "TTuple([#1, #1])");
}

#[test]
fn values_of_the_wrong_type_are_errors() {
    let mut heap = Heap::default();
    let s = heap.string("a string");
    let block = heap.block(9, &[int(1)]);
    let small = heap.block(2, &[]);
    let errors = [
        unsafe { convert::<Ref<type_::Ty>>(int(3)) }.map(drop),
        unsafe { convert::<Ref<type_::Ty>>(block) }.map(drop),
        unsafe { convert::<Ref<type_::Ty>>(small) }.map(drop),
        unsafe { convert::<Str>(int(1)) }.map(drop),
        unsafe { convert::<Str>(block) }.map(drop),
        unsafe { convert::<Vec<i64>>(s) }.map(drop),
        unsafe { convert::<crate::Nothing>(int(0)) }.map(drop),
    ];
    let errors: Vec<String> = errors.into_iter().map(|e| e.unwrap_err().0).collect();
    assert_eq!(
        errors,
        [
            "Type.ty: unexpected immediate 3",
            "Type.ty: unexpected block of tag 9 and size 1",
            "expected a block with field 0, got one of size 0",
            "string: unexpected immediate 1",
            "string: unexpected block of tag 9 and size 1",
            "list: unexpected block of tag 252 and size 2",
            "Nothing.t: unexpected immediate 0",
        ]
    );
}

/// [`crate::to_ocaml`] keeps the built values of each store at the
/// [`crate::Indexed::STORE`] position of [`crate::Arena::store_lens`].
#[test]
fn store_indices_match_store_lens() {
    use crate::{Arena, Indexed};
    fn check<T: Indexed>(arena: &Arena, seen: &mut Vec<usize>) {
        assert_eq!(arena.store_lens()[T::STORE], T::store(arena).len());
        seen.push(T::STORE);
    }
    let mut arena = Arena::default();
    // Distinct lengths, so that a store at the wrong index is noticed.
    for n in 0..3 {
        arena
            .type_ty
            .push(type_::Ty::TCon(n.to_string().into(), vec![]));
    }
    arena.type_row.push(type_::Row::REmpty);
    let mut seen = vec![];
    check::<crate::Prim>(&arena, &mut seen);
    check::<type_::Row>(&arena, &mut seen);
    check::<type_::Ty>(&arena, &mut seen);
    check::<type_::Var<type_::Presence, type_::PresenceBound>>(&arena, &mut seen);
    check::<type_::Var<Ref<type_::Row>, type_::Bound>>(&arena, &mut seen);
    check::<type_::Var<Ref<type_::Ty>, crate::Nothing>>(&arena, &mut seen);
    check::<crate::typed::RecordType>(&arena, &mut seen);
    seen.sort();
    assert_eq!(seen, (0..arena.store_lens().len()).collect::<Vec<_>>());
}

#[test]
fn strings_are_bytes() {
    let mut heap = Heap::default();
    let v = heap.bytes(b"\xff\x00\"\\\n\xc3\xa9 ok");
    let (s, _) = unsafe { convert::<Str>(v) }.unwrap();
    assert_eq!(s.as_bytes(), b"\xff\x00\"\\\n\xc3\xa9 ok");
    assert!(s.to_str().is_err());
    assert_eq!(format!("{s:?}"), r#""\255\000\"\\\n\195\169 ok""#);
    assert_eq!(format!("{s}"), "\u{fffd}\0\"\\\n\u{e9} ok");
    assert_eq!(Str::from("é").to_str(), Ok("é"));
}
