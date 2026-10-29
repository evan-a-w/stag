use crate::test_heap::{int, Heap};
use crate::{convert, type_, Map, Ref, Set};

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
    let (converted, _) = unsafe { convert::<Option<Vec<String>>>(some) }.unwrap();
    assert_eq!(converted, Some(strings.map(String::from).to_vec()));
    let (none, _) = unsafe { convert::<Option<Vec<String>>>(int(0)) }.unwrap();
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
    let (map, _) = unsafe { convert::<Map<i64, String>>(map) }.unwrap();
    assert_eq!(format!("{map:?}"), r#"{1: "a", 2: "b", 3: "c", 4: "d"}"#);
    assert_eq!(map.get(&3).map(String::as_str), Some("c"));
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

/// Deeper than the default stacks of threads allow.
#[test]
fn deep_values() {
    let mut heap = Heap::default();
    let var = var(&mut heap, 0);
    heap.set(var, 1, int(0));
    let mut ty = heap.block(6, &[var]);
    let depth = 100_000;
    for _ in 0..depth {
        ty = heap.block(1, &[ty, ty]);
    }
    let (_, arena) = unsafe { convert::<Ref<type_::Ty>>(ty) }.unwrap();
    assert_eq!(arena.type_ty.len(), depth + 1);
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
        unsafe { convert::<String>(int(1)) }.map(drop),
        unsafe { convert::<String>(block) }.map(drop),
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
