use crate::example::{defs, Ty};
use crate::test_heap::{int, Heap};
use crate::{convert, Ref};

#[test]
fn inline_records_skip_fields_and_arrays_may_be_empty() {
    let mut heap = Heap::default();
    let name = heap.string("r");
    let x = heap.string("x");
    let fields = heap.array(&[x]);
    let record_type = heap.block(0, &[name, fields, int(0), int(2)]);
    let field = heap.block(0, &[x, int(0)]);
    let fields = heap.list(&[field]);
    let record = heap.block(2, &[record_type, int(0), fields]);
    let empty = heap.array(&[]);
    let chars = heap.block(4, &[int(b'c' as isize), empty]);
    let pair = heap.block(0, &[record, chars]);
    let ((record, chars), arena) = unsafe { convert::<(Ref<Ty>, Ref<Ty>)>(pair) }.unwrap();
    assert_eq!(
        format!(
            "{:?} {:?} {:?}",
            arena[record], arena[chars], arena.example_record_type
        ),
        r#"Record { record: #0, fields: [("x", #1)] } Chars(99, []) {#0: RecordType { name: "r", fields: ["x"], arity: 2 }}"#
    );
}

#[test]
fn nothing() {
    let mut heap = Heap::default();
    let pos = heap.block(0, &[int(1), int(2)]);
    let absurd = heap.block(3, &[int(0)]);
    let expr = heap.block(0, &[absurd, pos]);
    let (expr, _) = unsafe { convert::<defs::Expr>(expr) }.unwrap();
    assert_eq!(
        format!("{expr:?}"),
        "Located { txt: Absurd([]), pos: Pos { line: 1, col: 2 } }"
    );
    let never = heap.block(2, &[int(0)]);
    let expr = heap.block(0, &[never, pos]);
    let error = unsafe { convert::<defs::Expr>(expr) }.unwrap_err();
    assert_eq!(error.0, "Nothing.t: unexpected immediate 0");
}
