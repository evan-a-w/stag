open! Import

let toplevels ?prelude source =
  match Source_helpers.typecheck ?prelude source with
  | None -> ()
  | Some t -> print_endline (Debug.typed_toplevels (Source_helpers.members t))
;;

let%expect_test "a generalised scheme shares its variable" =
  toplevels ~prelude:false "let id x = x";
  [%expect
    {|
    [
        Toplevel {
            ident: Ident {
                name: "id",
                stamp: 0,
            },
            args: [
                Ident {
                    name: "x",
                    stamp: 1,
                },
            ],
            scheme: #0,
            body: Expr {
                desc: Var(
                    Bound(
                        Local(
                            Ident {
                                name: "x",
                                stamp: 1,
                            },
                        ),
                    ),
                ),
                ty: #1,
                loc: Loc {
                    file: "t.stag",
                    start: Pos {
                        line: 1,
                        col: 11,
                        offset: 11,
                    },
                    stop: Pos {
                        line: 1,
                        col: 12,
                        offset: 12,
                    },
                },
            },
        },
    ]
    Arena {
        prim: {},
        type_row: {},
        type_ty: {
            #0: TArrow(
                #1,
                #2,
            ),
            #1: TVar(
                #0,
            ),
            #2: TVar(
                #1,
            ),
        },
        type_var_presence_presence_bound: {},
        type_var_row_bound: {},
        type_var_ty_nothing: {
            #0: Var {
                id: 1,
                state: Generic,
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #1: Var {
                id: 2,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        typed_record_type: {},
    }
    |}]
;;

let%expect_test "a cyclic type" =
  let var = Type.Var.create 7 (Unbound 0) in
  let ty = Type.TVar var in
  var.state <- Link (TArrow (ty, TTuple [ ty; TCon ("int", []) ]));
  print_endline (Debug.ty ty);
  [%expect
    {|
    #0
    Arena {
        prim: {},
        type_row: {},
        type_ty: {
            #0: TVar(
                #0,
            ),
            #1: TArrow(
                #0,
                #2,
            ),
            #2: TTuple(
                [
                    #0,
                    #3,
                ],
            ),
            #3: TCon(
                "int",
                [],
            ),
        },
        type_var_presence_presence_bound: {},
        type_var_row_bound: {},
        type_var_ty_nothing: {
            #0: Var {
                id: 7,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        typed_record_type: {},
    }
    |}]
;;

let%expect_test "records share their record type, prims are stored" =
  toplevels
    {|
type point = { x : int; y : int }
let p = { x = Int.add 1 2; y = 3 }
let x = p.x
|};
  [%expect
    {|
    [
        Toplevel {
            ident: Ident {
                name: "p",
                stamp: 4,
            },
            args: [],
            scheme: #0,
            body: Expr {
                desc: Record(
                    [
                        (
                            FieldRef {
                                record: #0,
                                index: 0,
                            },
                            Expr {
                                desc: App(
                                    Expr {
                                        desc: App(
                                            Expr {
                                                desc: Var(
                                                    Prim(
                                                        #0,
                                                    ),
                                                ),
                                                ty: #2,
                                                loc: Loc {
                                                    file: "t.stag",
                                                    start: Pos {
                                                        line: 3,
                                                        col: 14,
                                                        offset: 49,
                                                    },
                                                    stop: Pos {
                                                        line: 3,
                                                        col: 21,
                                                        offset: 56,
                                                    },
                                                },
                                            },
                                            Expr {
                                                desc: Lit(
                                                    Int(
                                                        1,
                                                    ),
                                                ),
                                                ty: #3,
                                                loc: Loc {
                                                    file: "t.stag",
                                                    start: Pos {
                                                        line: 3,
                                                        col: 22,
                                                        offset: 57,
                                                    },
                                                    stop: Pos {
                                                        line: 3,
                                                        col: 23,
                                                        offset: 58,
                                                    },
                                                },
                                            },
                                        ),
                                        ty: #5,
                                        loc: Loc {
                                            file: "t.stag",
                                            start: Pos {
                                                line: 3,
                                                col: 14,
                                                offset: 49,
                                            },
                                            stop: Pos {
                                                line: 3,
                                                col: 23,
                                                offset: 58,
                                            },
                                        },
                                    },
                                    Expr {
                                        desc: Lit(
                                            Int(
                                                2,
                                            ),
                                        ),
                                        ty: #3,
                                        loc: Loc {
                                            file: "t.stag",
                                            start: Pos {
                                                line: 3,
                                                col: 24,
                                                offset: 59,
                                            },
                                            stop: Pos {
                                                line: 3,
                                                col: 25,
                                                offset: 60,
                                            },
                                        },
                                    },
                                ),
                                ty: #6,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 14,
                                        offset: 49,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 25,
                                        offset: 60,
                                    },
                                },
                            },
                        ),
                        (
                            FieldRef {
                                record: #0,
                                index: 1,
                            },
                            Expr {
                                desc: Lit(
                                    Int(
                                        3,
                                    ),
                                ),
                                ty: #3,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 31,
                                        offset: 66,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 32,
                                        offset: 67,
                                    },
                                },
                            },
                        ),
                    ],
                ),
                ty: #1,
                loc: Loc {
                    file: "t.stag",
                    start: Pos {
                        line: 3,
                        col: 8,
                        offset: 43,
                    },
                    stop: Pos {
                        line: 3,
                        col: 34,
                        offset: 69,
                    },
                },
            },
        },
        Toplevel {
            ident: Ident {
                name: "x",
                stamp: 5,
            },
            args: [],
            scheme: #7,
            body: Expr {
                desc: Field(
                    Expr {
                        desc: Var(
                            Bound(
                                Toplevel(
                                    Ident {
                                        name: "p",
                                        stamp: 4,
                                    },
                                ),
                            ),
                        ),
                        ty: #0,
                        loc: Loc {
                            file: "t.stag",
                            start: Pos {
                                line: 4,
                                col: 8,
                                offset: 78,
                            },
                            stop: Pos {
                                line: 4,
                                col: 9,
                                offset: 79,
                            },
                        },
                    },
                    FieldRef {
                        record: #0,
                        index: 0,
                    },
                ),
                ty: #8,
                loc: Loc {
                    file: "t.stag",
                    start: Pos {
                        line: 4,
                        col: 8,
                        offset: 78,
                    },
                    stop: Pos {
                        line: 4,
                        col: 11,
                        offset: 81,
                    },
                },
            },
        },
    ]
    Arena {
        prim: {
            #0: Prim {
                name: "Int.add",
                args: [
                    Int,
                    Int,
                ],
                result: Int,
            },
        },
        type_row: {},
        type_ty: {
            #0: TVar(
                #0,
            ),
            #1: TCon(
                "point",
                [],
            ),
            #2: TArrow(
                #3,
                #4,
            ),
            #3: TCon(
                "int",
                [],
            ),
            #4: TArrow(
                #3,
                #3,
            ),
            #5: TVar(
                #1,
            ),
            #6: TVar(
                #2,
            ),
            #7: TVar(
                #3,
            ),
            #8: TCon(
                "int",
                [],
            ),
        },
        type_var_presence_presence_bound: {},
        type_var_row_bound: {},
        type_var_ty_nothing: {
            #0: Var {
                id: 17,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #1: Var {
                id: 21,
                state: Link(
                    #4,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #2: Var {
                id: 19,
                state: Link(
                    #3,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #3: Var {
                id: 23,
                state: Link(
                    #8,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        typed_record_type: {
            #0: RecordType {
                name: "point",
                fields: [
                    "x",
                    "y",
                ],
            },
        },
    }
    |}]
;;

let%expect_test "constructors, matches and polymorphic variants and records" =
  toplevels
    {|
type t = A | B of int * string
let f v = match v with A -> `None | B (n, _) -> `Some n
let g x = match x with `Some n -> n | _ -> 0
|};
  [%expect
    {|
    [
        Toplevel {
            ident: Ident {
                name: "f",
                stamp: 4,
            },
            args: [
                Ident {
                    name: "v",
                    stamp: 15,
                },
            ],
            scheme: #0,
            body: Expr {
                desc: Match {
                    scrutinee: Expr {
                        desc: Var(
                            Bound(
                                Local(
                                    Ident {
                                        name: "v",
                                        stamp: 15,
                                    },
                                ),
                            ),
                        ),
                        ty: #1,
                        loc: Loc {
                            file: "t.stag",
                            start: Pos {
                                line: 3,
                                col: 16,
                                offset: 48,
                            },
                            stop: Pos {
                                line: 3,
                                col: 17,
                                offset: 49,
                            },
                        },
                    },
                    cases: [
                        (
                            Pattern {
                                desc: Construct(
                                    ConstructorRef {
                                        type_name: "t",
                                        name: "A",
                                        index: 0,
                                    },
                                    [],
                                ),
                                ty: #2,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 23,
                                        offset: 55,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 24,
                                        offset: 56,
                                    },
                                },
                            },
                            Expr {
                                desc: PolyVariant(
                                    "None",
                                    None,
                                ),
                                ty: #4,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 28,
                                        offset: 60,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 33,
                                        offset: 65,
                                    },
                                },
                            },
                        ),
                        (
                            Pattern {
                                desc: Construct(
                                    ConstructorRef {
                                        type_name: "t",
                                        name: "B",
                                        index: 1,
                                    },
                                    [
                                        Pattern {
                                            desc: Var(
                                                Ident {
                                                    name: "n",
                                                    stamp: 16,
                                                },
                                            ),
                                            ty: #6,
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 39,
                                                    offset: 71,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 40,
                                                    offset: 72,
                                                },
                                            },
                                        },
                                        Pattern {
                                            desc: Any,
                                            ty: #8,
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 42,
                                                    offset: 74,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 43,
                                                    offset: 75,
                                                },
                                            },
                                        },
                                    ],
                                ),
                                ty: #2,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 36,
                                        offset: 68,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 44,
                                        offset: 76,
                                    },
                                },
                            },
                            Expr {
                                desc: PolyVariant(
                                    "Some",
                                    Some(
                                        Expr {
                                            desc: Var(
                                                Bound(
                                                    Local(
                                                        Ident {
                                                            name: "n",
                                                            stamp: 16,
                                                        },
                                                    ),
                                                ),
                                            ),
                                            ty: #6,
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 54,
                                                    offset: 86,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 55,
                                                    offset: 87,
                                                },
                                            },
                                        },
                                    ),
                                ),
                                ty: #10,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 48,
                                        offset: 80,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 55,
                                        offset: 87,
                                    },
                                },
                            },
                        ),
                    ],
                    exception_cases: [],
                },
                ty: #11,
                loc: Loc {
                    file: "t.stag",
                    start: Pos {
                        line: 3,
                        col: 10,
                        offset: 42,
                    },
                    stop: Pos {
                        line: 3,
                        col: 55,
                        offset: 87,
                    },
                },
            },
        },
        Toplevel {
            ident: Ident {
                name: "g",
                stamp: 5,
            },
            args: [
                Ident {
                    name: "x",
                    stamp: 17,
                },
            ],
            scheme: #12,
            body: Expr {
                desc: Match {
                    scrutinee: Expr {
                        desc: Var(
                            Bound(
                                Local(
                                    Ident {
                                        name: "x",
                                        stamp: 17,
                                    },
                                ),
                            ),
                        ),
                        ty: #13,
                        loc: Loc {
                            file: "t.stag",
                            start: Pos {
                                line: 4,
                                col: 16,
                                offset: 104,
                            },
                            stop: Pos {
                                line: 4,
                                col: 17,
                                offset: 105,
                            },
                        },
                    },
                    cases: [
                        (
                            Pattern {
                                desc: PolyVariant(
                                    "Some",
                                    Some(
                                        Pattern {
                                            desc: Var(
                                                Ident {
                                                    name: "n",
                                                    stamp: 18,
                                                },
                                            ),
                                            ty: #15,
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 4,
                                                    col: 29,
                                                    offset: 117,
                                                },
                                                stop: Pos {
                                                    line: 4,
                                                    col: 30,
                                                    offset: 118,
                                                },
                                            },
                                        },
                                    ),
                                ),
                                ty: #14,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 4,
                                        col: 23,
                                        offset: 111,
                                    },
                                    stop: Pos {
                                        line: 4,
                                        col: 30,
                                        offset: 118,
                                    },
                                },
                            },
                            Expr {
                                desc: Var(
                                    Bound(
                                        Local(
                                            Ident {
                                                name: "n",
                                                stamp: 18,
                                            },
                                        ),
                                    ),
                                ),
                                ty: #15,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 4,
                                        col: 34,
                                        offset: 122,
                                    },
                                    stop: Pos {
                                        line: 4,
                                        col: 35,
                                        offset: 123,
                                    },
                                },
                            },
                        ),
                        (
                            Pattern {
                                desc: Any,
                                ty: #18,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 4,
                                        col: 38,
                                        offset: 126,
                                    },
                                    stop: Pos {
                                        line: 4,
                                        col: 39,
                                        offset: 127,
                                    },
                                },
                            },
                            Expr {
                                desc: Lit(
                                    Int(
                                        0,
                                    ),
                                ),
                                ty: #16,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 4,
                                        col: 43,
                                        offset: 131,
                                    },
                                    stop: Pos {
                                        line: 4,
                                        col: 44,
                                        offset: 132,
                                    },
                                },
                            },
                        ),
                    ],
                    exception_cases: [],
                },
                ty: #19,
                loc: Loc {
                    file: "t.stag",
                    start: Pos {
                        line: 4,
                        col: 10,
                        offset: 98,
                    },
                    stop: Pos {
                        line: 4,
                        col: 44,
                        offset: 132,
                    },
                },
            },
        },
    ]
    Arena {
        prim: {},
        type_row: {
            #0: RExtend(
                "None",
                Present,
                #5,
                #1,
            ),
            #1: RVar(
                #0,
            ),
            #2: RExtend(
                "Some",
                Present,
                #6,
                #3,
            ),
            #3: RVar(
                #1,
            ),
            #4: RExtend(
                "Some",
                Present,
                #6,
                #5,
            ),
            #5: RVar(
                #2,
            ),
            #6: RExtend(
                "None",
                PVar(
                    #0,
                ),
                #5,
                #3,
            ),
            #7: RExtend(
                "Some",
                Present,
                #15,
                #8,
            ),
            #8: RVar(
                #3,
            ),
        },
        type_ty: {
            #0: TArrow(
                #1,
                #3,
            ),
            #1: TVar(
                #0,
            ),
            #2: TCon(
                "t",
                [],
            ),
            #3: TVar(
                #1,
            ),
            #4: TVariant {
                id: 21,
                row: #0,
            },
            #5: TCon(
                "unit",
                [],
            ),
            #6: TVar(
                #2,
            ),
            #7: TCon(
                "int",
                [],
            ),
            #8: TVar(
                #3,
            ),
            #9: TCon(
                "string",
                [],
            ),
            #10: TVariant {
                id: 25,
                row: #4,
            },
            #11: TVar(
                #4,
            ),
            #12: TArrow(
                #13,
                #17,
            ),
            #13: TVar(
                #5,
            ),
            #14: TVariant {
                id: 34,
                row: #7,
            },
            #15: TVar(
                #6,
            ),
            #16: TCon(
                "int",
                [],
            ),
            #17: TVar(
                #7,
            ),
            #18: TVar(
                #8,
            ),
            #19: TVar(
                #9,
            ),
        },
        type_var_presence_presence_bound: {
            #0: Var {
                id: 26,
                state: Link(
                    Present,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        type_var_row_bound: {
            #0: Var {
                id: 20,
                state: Link(
                    #2,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #1: Var {
                id: 27,
                state: Generic,
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #2: Var {
                id: 24,
                state: Link(
                    #6,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #3: Var {
                id: 33,
                state: Generic,
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        type_var_ty_nothing: {
            #0: Var {
                id: 17,
                state: Link(
                    #2,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #1: Var {
                id: 18,
                state: Link(
                    #4,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #2: Var {
                id: 22,
                state: Link(
                    #7,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #3: Var {
                id: 23,
                state: Link(
                    #9,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #4: Var {
                id: 19,
                state: Link(
                    #4,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #5: Var {
                id: 29,
                state: Link(
                    #14,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #6: Var {
                id: 32,
                state: Link(
                    #16,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #7: Var {
                id: 30,
                state: Link(
                    #16,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #8: Var {
                id: 35,
                state: Link(
                    #14,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #9: Var {
                id: 31,
                state: Link(
                    #16,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        typed_record_type: {},
    }
    |}]
;;

let%expect_test "quotes, splices, expect tests and benches" =
  toplevels
    {xx|
let two = .~(.< 1 >.)
let%expect_test "e" = print_int two; [%expect {| 1 |}]
let%bench "b" n in [1; 2] = n
|xx};
  [%expect
    {|
    [
        Toplevel {
            ident: Ident {
                name: "two",
                stamp: 4,
            },
            args: [],
            scheme: #0,
            body: Expr {
                desc: ToplevelSplice {
                    code: Expr {
                        desc: Quote(
                            Expr {
                                desc: Lit(
                                    Int(
                                        1,
                                    ),
                                ),
                                ty: #1,
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 2,
                                        col: 16,
                                        offset: 17,
                                    },
                                    stop: Pos {
                                        line: 2,
                                        col: 17,
                                        offset: 18,
                                    },
                                },
                            },
                        ),
                        ty: #2,
                        loc: Loc {
                            file: "t.stag",
                            start: Pos {
                                line: 2,
                                col: 12,
                                offset: 13,
                            },
                            stop: Pos {
                                line: 2,
                                col: 21,
                                offset: 22,
                            },
                        },
                    },
                    expansion: Expr {
                        desc: Lit(
                            Int(
                                1,
                            ),
                        ),
                        ty: #1,
                        loc: Loc {
                            file: "t.stag",
                            start: Pos {
                                line: 2,
                                col: 16,
                                offset: 17,
                            },
                            stop: Pos {
                                line: 2,
                                col: 17,
                                offset: 18,
                            },
                        },
                    },
                },
                ty: #3,
                loc: Loc {
                    file: "t.stag",
                    start: Pos {
                        line: 2,
                        col: 10,
                        offset: 11,
                    },
                    stop: Pos {
                        line: 2,
                        col: 21,
                        offset: 22,
                    },
                },
            },
        },
    ]
    Arena {
        prim: {},
        type_row: {
            #0: RVar(
                #0,
            ),
            #1: REmpty,
        },
        type_ty: {
            #0: TVar(
                #0,
            ),
            #1: TCon(
                "int",
                [],
            ),
            #2: TCode {
                inner: #1,
                context: #0,
            },
            #3: TVar(
                #1,
            ),
        },
        type_var_presence_presence_bound: {},
        type_var_row_bound: {
            #0: Var {
                id: 21,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        type_var_ty_nothing: {
            #0: Var {
                id: 17,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
            #1: Var {
                id: 18,
                state: Link(
                    #1,
                ),
                bounds: [],
                in_bounds_of: [],
                kept: [],
            },
        },
        typed_record_type: {},
    }
    |}]
;;

let%expect_test "syntax" =
  print_endline
    (Debug.syntax_program
       (Source_helpers.parse_exn
          {|
type t['a] = A of 'a | B of int
let f (x : t[int]) = match x with A _ -> .< 'c' >. | B n -> .< n >.
|}));
  [%expect
    {|
    [
        Located {
            txt: Type(
                TypeDecl {
                    name: Located {
                        txt: "t",
                        loc: Loc {
                            file: "t.stag",
                            start: Pos {
                                line: 2,
                                col: 5,
                                offset: 6,
                            },
                            stop: Pos {
                                line: 2,
                                col: 6,
                                offset: 7,
                            },
                        },
                    },
                    params: [
                        Located {
                            txt: Ty(
                                "a",
                            ),
                            loc: Loc {
                                file: "t.stag",
                                start: Pos {
                                    line: 2,
                                    col: 7,
                                    offset: 8,
                                },
                                stop: Pos {
                                    line: 2,
                                    col: 9,
                                    offset: 10,
                                },
                            },
                        },
                    ],
                    kind: Variant(
                        [
                            (
                                Located {
                                    txt: "A",
                                    loc: Loc {
                                        file: "t.stag",
                                        start: Pos {
                                            line: 2,
                                            col: 13,
                                            offset: 14,
                                        },
                                        stop: Pos {
                                            line: 2,
                                            col: 14,
                                            offset: 15,
                                        },
                                    },
                                },
                                [
                                    Located {
                                        txt: Var(
                                            "a",
                                        ),
                                        loc: Loc {
                                            file: "t.stag",
                                            start: Pos {
                                                line: 2,
                                                col: 18,
                                                offset: 19,
                                            },
                                            stop: Pos {
                                                line: 2,
                                                col: 20,
                                                offset: 21,
                                            },
                                        },
                                    },
                                ],
                            ),
                            (
                                Located {
                                    txt: "B",
                                    loc: Loc {
                                        file: "t.stag",
                                        start: Pos {
                                            line: 2,
                                            col: 23,
                                            offset: 24,
                                        },
                                        stop: Pos {
                                            line: 2,
                                            col: 24,
                                            offset: 25,
                                        },
                                    },
                                },
                                [
                                    Located {
                                        txt: Con(
                                            Located {
                                                txt: Path(
                                                    [
                                                        "int",
                                                    ],
                                                ),
                                                loc: Loc {
                                                    file: "t.stag",
                                                    start: Pos {
                                                        line: 2,
                                                        col: 28,
                                                        offset: 29,
                                                    },
                                                    stop: Pos {
                                                        line: 2,
                                                        col: 31,
                                                        offset: 32,
                                                    },
                                                },
                                            },
                                            [],
                                        ),
                                        loc: Loc {
                                            file: "t.stag",
                                            start: Pos {
                                                line: 2,
                                                col: 28,
                                                offset: 29,
                                            },
                                            stop: Pos {
                                                line: 2,
                                                col: 31,
                                                offset: 32,
                                            },
                                        },
                                    },
                                ],
                            ),
                        ],
                    ),
                },
            ),
            loc: Loc {
                file: "t.stag",
                start: Pos {
                    line: 2,
                    col: 0,
                    offset: 1,
                },
                stop: Pos {
                    line: 2,
                    col: 31,
                    offset: 32,
                },
            },
        },
        Located {
            txt: Let(
                Located {
                    txt: "f",
                    loc: Loc {
                        file: "t.stag",
                        start: Pos {
                            line: 3,
                            col: 4,
                            offset: 37,
                        },
                        stop: Pos {
                            line: 3,
                            col: 5,
                            offset: 38,
                        },
                    },
                },
                [
                    (
                        Located {
                            txt: "x",
                            loc: Loc {
                                file: "t.stag",
                                start: Pos {
                                    line: 3,
                                    col: 7,
                                    offset: 40,
                                },
                                stop: Pos {
                                    line: 3,
                                    col: 8,
                                    offset: 41,
                                },
                            },
                        },
                        Some(
                            Located {
                                txt: Con(
                                    Located {
                                        txt: Path(
                                            [
                                                "t",
                                            ],
                                        ),
                                        loc: Loc {
                                            file: "t.stag",
                                            start: Pos {
                                                line: 3,
                                                col: 11,
                                                offset: 44,
                                            },
                                            stop: Pos {
                                                line: 3,
                                                col: 12,
                                                offset: 45,
                                            },
                                        },
                                    },
                                    [
                                        Ty(
                                            Located {
                                                txt: Con(
                                                    Located {
                                                        txt: Path(
                                                            [
                                                                "int",
                                                            ],
                                                        ),
                                                        loc: Loc {
                                                            file: "t.stag",
                                                            start: Pos {
                                                                line: 3,
                                                                col: 13,
                                                                offset: 46,
                                                            },
                                                            stop: Pos {
                                                                line: 3,
                                                                col: 16,
                                                                offset: 49,
                                                            },
                                                        },
                                                    },
                                                    [],
                                                ),
                                                loc: Loc {
                                                    file: "t.stag",
                                                    start: Pos {
                                                        line: 3,
                                                        col: 13,
                                                        offset: 46,
                                                    },
                                                    stop: Pos {
                                                        line: 3,
                                                        col: 16,
                                                        offset: 49,
                                                    },
                                                },
                                            },
                                        ),
                                    ],
                                ),
                                loc: Loc {
                                    file: "t.stag",
                                    start: Pos {
                                        line: 3,
                                        col: 11,
                                        offset: 44,
                                    },
                                    stop: Pos {
                                        line: 3,
                                        col: 17,
                                        offset: 50,
                                    },
                                },
                            },
                        ),
                    ),
                ],
                None,
                Located {
                    txt: Match(
                        Located {
                            txt: Var(
                                Path(
                                    [
                                        "x",
                                    ],
                                ),
                                None,
                            ),
                            loc: Loc {
                                file: "t.stag",
                                start: Pos {
                                    line: 3,
                                    col: 27,
                                    offset: 60,
                                },
                                stop: Pos {
                                    line: 3,
                                    col: 28,
                                    offset: 61,
                                },
                            },
                        },
                        [
                            (
                                Located {
                                    txt: Construct(
                                        Located {
                                            txt: Path(
                                                [
                                                    "A",
                                                ],
                                            ),
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 34,
                                                    offset: 67,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 35,
                                                    offset: 68,
                                                },
                                            },
                                        },
                                        [
                                            Located {
                                                txt: Any,
                                                loc: Loc {
                                                    file: "t.stag",
                                                    start: Pos {
                                                        line: 3,
                                                        col: 36,
                                                        offset: 69,
                                                    },
                                                    stop: Pos {
                                                        line: 3,
                                                        col: 37,
                                                        offset: 70,
                                                    },
                                                },
                                            },
                                        ],
                                    ),
                                    loc: Loc {
                                        file: "t.stag",
                                        start: Pos {
                                            line: 3,
                                            col: 34,
                                            offset: 67,
                                        },
                                        stop: Pos {
                                            line: 3,
                                            col: 37,
                                            offset: 70,
                                        },
                                    },
                                },
                                Located {
                                    txt: Quote(
                                        Located {
                                            txt: Lit(
                                                Char(
                                                    99,
                                                ),
                                            ),
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 44,
                                                    offset: 77,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 47,
                                                    offset: 80,
                                                },
                                            },
                                        },
                                    ),
                                    loc: Loc {
                                        file: "t.stag",
                                        start: Pos {
                                            line: 3,
                                            col: 41,
                                            offset: 74,
                                        },
                                        stop: Pos {
                                            line: 3,
                                            col: 50,
                                            offset: 83,
                                        },
                                    },
                                },
                            ),
                            (
                                Located {
                                    txt: Construct(
                                        Located {
                                            txt: Path(
                                                [
                                                    "B",
                                                ],
                                            ),
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 53,
                                                    offset: 86,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 54,
                                                    offset: 87,
                                                },
                                            },
                                        },
                                        [
                                            Located {
                                                txt: Var(
                                                    "n",
                                                ),
                                                loc: Loc {
                                                    file: "t.stag",
                                                    start: Pos {
                                                        line: 3,
                                                        col: 55,
                                                        offset: 88,
                                                    },
                                                    stop: Pos {
                                                        line: 3,
                                                        col: 56,
                                                        offset: 89,
                                                    },
                                                },
                                            },
                                        ],
                                    ),
                                    loc: Loc {
                                        file: "t.stag",
                                        start: Pos {
                                            line: 3,
                                            col: 53,
                                            offset: 86,
                                        },
                                        stop: Pos {
                                            line: 3,
                                            col: 56,
                                            offset: 89,
                                        },
                                    },
                                },
                                Located {
                                    txt: Quote(
                                        Located {
                                            txt: Var(
                                                Path(
                                                    [
                                                        "n",
                                                    ],
                                                ),
                                                None,
                                            ),
                                            loc: Loc {
                                                file: "t.stag",
                                                start: Pos {
                                                    line: 3,
                                                    col: 63,
                                                    offset: 96,
                                                },
                                                stop: Pos {
                                                    line: 3,
                                                    col: 64,
                                                    offset: 97,
                                                },
                                            },
                                        },
                                    ),
                                    loc: Loc {
                                        file: "t.stag",
                                        start: Pos {
                                            line: 3,
                                            col: 60,
                                            offset: 93,
                                        },
                                        stop: Pos {
                                            line: 3,
                                            col: 67,
                                            offset: 100,
                                        },
                                    },
                                },
                            ),
                        ],
                    ),
                    loc: Loc {
                        file: "t.stag",
                        start: Pos {
                            line: 3,
                            col: 21,
                            offset: 54,
                        },
                        stop: Pos {
                            line: 3,
                            col: 67,
                            offset: 100,
                        },
                    },
                },
            ),
            loc: Loc {
                file: "t.stag",
                start: Pos {
                    line: 3,
                    col: 0,
                    offset: 33,
                },
                stop: Pos {
                    line: 3,
                    col: 67,
                    offset: 100,
                },
            },
        },
    ]
    Arena {
        prim: {},
        type_row: {},
        type_ty: {},
        type_var_presence_presence_bound: {},
        type_var_row_bound: {},
        type_var_ty_nothing: {},
        typed_record_type: {},
    }
    |}]
;;

let%expect_test "a whole program, with the prelude" =
  match
    Source_helpers.typecheck {| let%expect_test "a" = print_int (Int.add 1 2) |}
  with
  | None -> ()
  | Some t ->
    print_endline
      (Debug.typed_program_summary (Source_helpers.checked t).program);
    [%expect
      {|
      sccs: 4
      expect_tests: 1
      benches: 0
      ident_info: 13
      arena: prims 2, record types 0, tys 33, rows 0, ty vars 12, row vars 0, presence vars 0
      |}]
;;

let%expect_test "a map" =
  match
    Source_helpers.typecheck
      ~prelude:false
      {|
let a = 1
let b = a
let c x = x
|}
  with
  | None -> ()
  | Some t ->
    print_endline (Debug.typed_ident_info (Source_helpers.checked t).program);
    [%expect
      {|
      {
          Ident {
              name: "a",
              stamp: 0,
          }: IdentInfo {
              ty: #0,
              level: 0,
          },
          Ident {
              name: "b",
              stamp: 1,
          }: IdentInfo {
              ty: #2,
              level: 0,
          },
          Ident {
              name: "c",
              stamp: 2,
          }: IdentInfo {
              ty: #3,
              level: 0,
          },
          Ident {
              name: "x",
              stamp: 3,
          }: IdentInfo {
              ty: #4,
              level: 0,
          },
      }
      |}]
;;

let%expect_test "a set, in a row variable's bound" =
  let row_var = Type.Var.create 1 (Unbound 0) in
  row_var.bounds
  <- [ { Type.row = REmpty
       ; shadowed = String.Set.of_list [ "e"; "d"; "c"; "b"; "a" ]
       }
     ];
  print_endline (Debug.ty (TRecord { id = 2; row = RVar row_var }));
  [%expect
    {|
    #0
    Arena {
        prim: {},
        type_row: {
            #0: RVar(
                #0,
            ),
            #1: REmpty,
        },
        type_ty: {
            #0: TRecord {
                id: 2,
                row: #0,
            },
        },
        type_var_presence_presence_bound: {},
        type_var_row_bound: {
            #0: Var {
                id: 1,
                state: Unbound(
                    0,
                ),
                bounds: [
                    Bound {
                        row: #1,
                        shadowed: {
                            "a",
                            "b",
                            "c",
                            "d",
                            "e",
                        },
                    },
                ],
                in_bounds_of: [],
                kept: [],
            },
        },
        type_var_ty_nothing: {},
        typed_record_type: {},
    }
    |}]
;;

let%expect_test "types shared as a DAG stay shared" =
  let ty =
    Fn.apply_n_times
      ~n:64
      (fun ty -> Type.TTuple [ ty; ty ])
      (Type.TCon ("int", []))
  in
  print_endline (Debug.ty_count ty);
  [%expect {| tys: 65 |}]
;;

let%expect_test "idents minted in Rust share the typechecker's stamps" =
  let stamps = Stag_common.Stamp_counter.create () in
  let ocaml () = Stag_common.Stamp_counter.next stamps in
  let before = ocaml () in
  let rust = Debug.fresh_idents stamps ~name:"x" ~n:3 in
  let after = ocaml () in
  print_s [%message (before : int) rust (after : int)];
  [%expect {|
    ((before 0)
     "[Ident { name: \"x\", stamp: 1 }, Ident { name: \"x\", stamp: 2 }, Ident { name: \"x\", stamp: 3 }]"
     (after 4))
    |}]
;;
