use expect_test::expect;

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
    let (some, _) = unsafe { convert::<Option<Vec<Str>>>(some) }.unwrap();
    let (none, _) = unsafe { convert::<Option<Vec<Str>>>(int(0)) }.unwrap();
    expect![[r#"
        Some(["", "a", "1234567", "12345678", "a longer string"])
        None
    "#]]
    .assert_eq(&format!("{some:?}\n{none:?}\n"));
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

    let leaf = heap.block(0, &[int(5)]);
    let tree = heap.block(1, &[int(0), int(6), leaf, int(2), int(2)]);
    let set = heap.block(0, &[int(0), tree]);
    let (set, _) = unsafe { convert::<Set<i64>>(set) }.unwrap();

    let empty = heap.block(0, &[int(0), int(0)]);
    let (empty, _) = unsafe { convert::<Set<i64>>(empty) }.unwrap();
    expect![[r#"
        map: {1: "a", 2: "b", 3: "c", 4: "d"}
        get 3: Some("c")
        get 5: None
        set: {5, 6}
        contains 5: true
        contains 4: false
        empty: {}
    "#]].assert_eq(&format!(
        "map: {map:?}\nget 3: {:?}\nget 5: {:?}\nset: {set:?}\ncontains 5: {}\ncontains 4: {}\nempty: {empty:?}\n",
        map.get(&3),
        map.get(&5),
        set.contains(&5),
        set.contains(&4),
    ));
}

/// `{ id; state; bounds = []; in_bounds_of = []; kept = []; in_record = false }`, with its state
/// to be set.
fn var(heap: &mut Heap, id: isize) -> crate::raw::Value {
    heap.block(0, &[int(id), int(0), int(0), int(0), int(0), int(0)])
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
    let (pair, arena) = unsafe { convert::<(Ref<type_::Ty>, Ref<type_::Ty>)>(pair) }.unwrap();
    expect![[r#"
        (#0, #0)
        {
            #0: TVar(
                #0,
            ),
            #1: TArrow(
                #0,
                #0,
            ),
        }
        {
            #0: Var {
                id: 7,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
                in_record: false,
            },
        }
    "#]]
    .assert_eq(&format!(
        "{pair:?}\n{:#?}\n{:#?}\n",
        arena.type_ty, arena.type_var_ty_nothing
    ));
}

#[test]
fn conversions_into_one_arena_append() {
    let mut heap = Heap::default();
    let first = var(&mut heap, 1);
    let first = heap.block(6, &[first]);
    let second = var(&mut heap, 2);
    let second = heap.block(6, &[second]);
    let mut arena = Arena::default();
    let mut out = String::new();
    for v in [first, second, first, int(0)] {
        let r = unsafe { convert_into::<Ref<type_::Ty>>(v, &mut arena) };
        out += &format!("{r:?}\n");
    }
    out += &format!("{:#?}\n{:#?}\n", arena.type_ty, arena.type_var_ty_nothing);
    expect![[r#"
        Ok(#0)
        Ok(#1)
        Ok(#2)
        Err(Error("Type.ty: unexpected immediate 0"))
        {
            #0: TVar(
                #0,
            ),
            #1: TVar(
                #1,
            ),
            #2: TVar(
                #2,
            ),
        }
        {
            #0: Var {
                id: 1,
                state: Generic,
                bounds: [],
                in_bounds_of: [],
                kept: [],
                in_record: false,
            },
            #1: Var {
                id: 2,
                state: Generic,
                bounds: [],
                in_bounds_of: [],
                kept: [],
                in_record: false,
            },
            #2: Var {
                id: 1,
                state: Generic,
                bounds: [],
                in_bounds_of: [],
                kept: [],
                in_record: false,
            },
        }
    "#]]
    .assert_eq(&out);
}

/// `TTuple [t; t]` nested 64 times around a var: as shared as it is in OCaml,
/// rather than `2^64` types.
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
    expect![[r#"
        #0: TTuple([#1, #1])
        types: 65
    "#]]
    .assert_eq(&format!(
        "{ty:?}: {:?}\ntypes: {}\n",
        arena[ty],
        arena.type_ty.len()
    ));
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
    let out: String = errors.iter().map(|e| format!("{e:?}\n")).collect();
    expect![[r#"
        Err(Error("Type.ty: unexpected immediate 3"))
        Err(Error("Type.ty: unexpected block of tag 9 and size 1"))
        Err(Error("expected a block with field 0, got one of size 0"))
        Err(Error("string: unexpected immediate 1"))
        Err(Error("string: unexpected block of tag 9 and size 1"))
        Err(Error("list: unexpected block of tag 252 and size 2"))
        Err(Error("Nothing.t: unexpected immediate 0"))
    "#]]
    .assert_eq(&out);
}

/// [`crate::to_ocaml`] keeps the built values of each store at the
/// [`crate::Indexed::STORE`] position of [`crate::Arena::slot_counts`].
#[test]
fn store_indices_match_slot_counts() {
    use crate::{Arena, Indexed};
    fn row<T: Indexed>(arena: &Arena, name: &str) -> String {
        format!("{} {name}: {}\n", T::STORE, T::store(arena).slot_count())
    }
    let mut arena = Arena::default();
    // Distinct lengths, so that a store at the wrong index is noticed.
    for n in 0..3 {
        arena
            .type_ty
            .insert(type_::Ty::TCon(n.to_string().into(), vec![]));
    }
    arena.type_row.insert(type_::Row::REmpty);
    let mut rows = [
        row::<crate::Prim>(&arena, "Prim"),
        row::<type_::Row>(&arena, "Row"),
        row::<type_::Ty>(&arena, "Ty"),
        row::<type_::Var<type_::Presence, type_::PresenceBound>>(&arena, "presence var"),
        row::<type_::Var<Ref<type_::Row>, type_::Bound>>(&arena, "row var"),
        row::<type_::Var<Ref<type_::Ty>, crate::Nothing>>(&arena, "ty var"),
        row::<crate::typed::RecordType>(&arena, "RecordType"),
    ];
    rows.sort();
    expect![[r#"
        slot_counts: [0, 1, 3, 0, 0, 0, 0]
        0 Prim: 0
        1 Row: 1
        2 Ty: 3
        3 presence var: 0
        4 row var: 0
        5 ty var: 0
        6 RecordType: 0
    "#]]
    .assert_eq(&format!(
        "slot_counts: {:?}\n{}",
        arena.slot_counts(),
        rows.concat()
    ));
}

#[test]
fn strings_are_bytes() {
    let mut heap = Heap::default();
    let v = heap.bytes(b"\xff\x00\"\\\n\xc3\xa9 ok");
    let (s, _) = unsafe { convert::<Str>(v) }.unwrap();
    expect![[r#"
        bytes: [255, 0, 34, 92, 10, 195, 169, 32, 111, 107]
        valid utf-8: false
        debug: "\255\000\"\\\n\195\169 ok"
        display: "�\0\"\\\né ok"
        to_str of "é": Ok("é")
    "#]]
    .assert_eq(&format!(
        "bytes: {:?}\nvalid utf-8: {:?}\ndebug: {s:?}\ndisplay: {:?}\nto_str of \"é\": {:?}\n",
        s.as_bytes(),
        s.to_str().is_ok(),
        s.to_string(),
        Str::from("é").to_str()
    ));
}

#[test]
fn stores_reuse_removed_slots() {
    use crate::Store;
    let mut store = Store::default();
    let mut log = String::new();
    let mut show = |what: &str, store: &Store<&str>| {
        log += &format!("{what}: {store:?} (len {})\n", store.len());
    };
    let a = store.insert("a");
    let b = store.insert("b");
    let c = store.insert("c");
    show("inserted a b c", &store);
    let removed = (store.remove(b), store.remove(b), store.remove(a));
    show(&format!("removed b, b again, a: {removed:?}"), &store);
    let (got_a, got_c) = (store.get(a), store.get(c));
    show(&format!("get a, c: {got_a:?} {got_c:?}"), &store);
    let d = store.insert("d");
    let e = store.insert("e");
    let f = store.insert("f");
    show(&format!("inserted d e f at {d:?} {e:?} {f:?}"), &store);
    store[c] = "C";
    store.retain(|_, x| *x != "e");
    show("set c to C, retained all but e", &store);
    let g = store.insert("g");
    show(&format!("inserted g at {g:?}"), &store);
    expect![[r#"
        inserted a b c: {#0: "a", #1: "b", #2: "c"} (len 3)
        removed b, b again, a: (Some("b"), None, Some("a")): {#2: "c"} (len 1)
        get a, c: None Some("c"): {#2: "c"} (len 1)
        inserted d e f at #0 #1 #3: {#0: "d", #1: "e", #2: "c", #3: "f"} (len 4)
        set c to C, retained all but e: {#0: "d", #2: "C", #3: "f"} (len 3)
        inserted g at #1: {#0: "d", #1: "g", #2: "C", #3: "f"} (len 4)
    "#]]
    .assert_eq(&log);
}

#[test]
fn converting_appends_after_free_slots() {
    use crate::{Pending, Store};
    let mut store = Store::default();
    let a = store.insert("a");
    store.insert("b");
    store.remove(a);
    let mut pending = Pending::after(&store);
    let (x, _) = pending.reserve(0x100);
    let (y, _) = pending.reserve(0x200);
    let (x_again, new) = pending.reserve(0x100);
    pending.fill(x, "x");
    pending.fill(y, "y");
    pending.finish_into(&mut store);
    let reused = store.insert("c");
    expect![[r#"
        reserved #2 #3, #2 again (new: false)
        inserted c at #0
        {#0: "c", #1: "b", #2: "x", #3: "y"} (len 4)
    "#]].assert_eq(&format!(
        "reserved {x:?} {y:?}, {x_again:?} again (new: {new})\ninserted c at {reused:?}\n{store:?} (len {})\n",
        store.len()
    ));
}
