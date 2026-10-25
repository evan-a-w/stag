You should prefer defining types as a [type t] inside a module with the name of
the type. Eg. [type undo_entry = ...] should be [module Undo_entry = struct type
t = ... end].

Always run ocamlformat after you finish (eg. ocamlformat --inplace */*.ml; ocamlformat */*.mli).

Prefer to write explicit interfaces for new files in the mli, rather than just
creating an ml file. If the mli and the ml file duplicate definitions of module
types (eg. for functors), use a \*_intf.ml file where you define the module types
and the resulting mli module type, include that from the ml, and make the mli simply [include X_intf.X] for X.mli

Prefer to write tests in expect test style (let%expect_test ..., prefer printing sexps to demonstrate state rather than eg. asserting things are equal to some manually defined value, look at the output of dune build @runtest, and if diffs are acceptable run dune promote to accept the changes)

Prefer to modularize code where possible. For instance, if there is functionality that is self contained, you should create a file module.ml with a [type t] defined inside, and expose only the minimal necessary functionality in the .mli file.

Eagerly remove dead code, unless you think it has a good chance of being useful
in future, and it doesn't add complexity / performance losses / prevent optimisations.

For common modules in Core, like [Table], [Set] etc., you should refer the re-exported/functor applied modules, like [Int.Table.t]. [Int.Table] (and equivalent fo [Set], [Map], etc.) have functions specific to the [Int] table, like [create], [of_list], etc., but not the functions that are the same for all tables (eg. find, where instead you need [Hashtbl.find], [Map.find] etc.). This applies everywhere these generic [Core] modules exist, and you should use them this way, rather than doing stuff like [Hashtbl.create (module Int)] or referring to the type as [(int, int) Hashtbl.t].

Types should typically be in a module like [Type] and be [Type.t], rather than [type type_].

Use the import.ml module to import things.

Only write comments for things that are truly hard to understand without them.
Don't just restate things that can be intuited based on context, names, code, etc.

If you see that I have changed a module, do not revert those changes, but apply
the ideas elsewhere. For instance, I might change the API slightly to make it
cleaner, or remove/reword comments, and you should respect that.

Don't write useless comments.

Test comprehensively, such that we can be fully confident in correctness, and
don't consider work done until tests are present and working. Prefer
having human readable test output (in expect test style) rather than checking
(eg. don't just [assert_true] some condition, print something demonstrating it).

Use subagents to
1. Parallelize work, where relevant
2. Just delegate, so that you are more responsive to the user - you can wait on subagent output and therefore in the meantime while it works, be ready to respond to the user. This is important.
3. Review code (independent subagents are more likely to catch issues). You should do this for nontrivial work before reporting to the user that it's done. Don't duplicate this though - eg. if you are a subagent spawned by a different agent, let the parent agent start any reviewers.

Keep commit messages just as "😊"
