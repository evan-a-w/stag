use expect_test::expect;

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
    expect![[r#"
        Record { record: #0, fields: [("x", #1)] }
        Chars(99, [])
        {
            #0: RecordType {
                name: "r",
                fields: [
                    "x",
                ],
                arity: 2,
            },
        }
    "#]]
    .assert_eq(&format!(
        "{:?}\n{:?}\n{:#?}\n",
        arena[record], arena[chars], arena.example_record_type
    ));
}

#[test]
fn nothing() {
    let mut heap = Heap::default();
    let pos = heap.block(0, &[int(1), int(2)]);
    let absurd = heap.block(3, &[int(0)]);
    let absurd = heap.block(0, &[absurd, pos]);
    let never = heap.block(2, &[int(0)]);
    let never = heap.block(0, &[never, pos]);
    let out: String = [absurd, never]
        .into_iter()
        .map(|v| {
            format!(
                "{:?}\n",
                unsafe { convert::<defs::Expr>(v) }.map(|(e, _)| e)
            )
        })
        .collect();
    expect![[r#"
        Ok(Located { txt: Absurd([]), pos: Pos { line: 1, col: 2 } })
        Err(Error("Nothing.t: unexpected immediate 0"))
    "#]]
    .assert_eq(&out);
}
