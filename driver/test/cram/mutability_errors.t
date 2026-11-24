Messages of errors about mutable variables.

  $ cat > a.stag <<'X'
  > let ok2 x = let g u = x in .<g ()>.
  > let fn (h : unit -> int) = let k = h in let g u = k () in .<g ()>.
  > let direct = let mut n = 0 in let f u = n in let g u = f () in .<g ()>.
  > let fixed (x : int) = let g u = x in .<g ()>.
  > let x = f 1
  > let f n = Int.add x n
  > let mut counter = 0
  > let incr u = counter <- Int.add counter 1
  > let at_compile_time = .~(incr (); .<1>.)
  > let immutable = let y = 1 in y <- 2
  > let assigns = .<z <- 1>.
  > let plain = .<let z = 0 in .~assigns>.
  > let by_expr = .<.~assigns.{z = 1}>.
  > let annotated : <unit>{z : immut int} = assigns
  > X
  $ stag check a.stag
  File "a.stag", line 1, characters 29-30:
  1 | let ok2 x = let g u = x in .<g ()>.
                                   ^
  Error: The quote can't embed g, as it may not be portable. Bind it as (g @ portable) to embed it, or write [forget g in ...] to make g a free variable of the quote
  File "a.stag", line 2, characters 60-61:
  2 | let fn (h : unit -> int) = let k = h in let g u = k () in .<g ()>.
                                                                  ^
  Error: The quote can't embed g, as it may not be portable. Bind it as (g @ portable) to embed it, or write [forget g in ...] to make g a free variable of the quote
  File "a.stag", line 3, characters 65-66:
  3 | let direct = let mut n = 0 in let f u = n in let g u = f () in .<g ()>.
                                                                       ^
  Error: The quote can't embed g, as it isn't portable. Write [forget g in ...] to make g a free variable of the quote
  File "a.stag", line 5, characters 8-9:
  5 | let x = f 1
              ^
  Error: The value of A.x may be needed to compute itself: A.x -> A.f -> A.x
  File "a.stag", line 9, characters 25-29:
  9 | let at_compile_time = .~(incr (); .<1>.)
                               ^^^^
  Error: The toplevel A.incr is used at compile time, but it may use the mutable toplevel A.counter when run: mutable toplevels exist only at runtime
  File "a.stag", line 10, characters 29-35:
  10 | let immutable = let y = 1 in y <- 2
                                    ^^^^^^
  Error: The variable y is not mutable: only one bound by [let mut] can be assigned to
  File "a.stag", line 12, characters 27-36:
  12 | let plain = .<let z = 0 in .~assigns>.
                                  ^^^^^^^^^
  Error: The spliced code assigns z (its context has z : mut ...), so it must be provided by a variable bound by [let mut] or a mutable toplevel
  File "a.stag", line 13, characters 16-33:
  13 | let by_expr = .<.~assigns.{z = 1}>.
                       ^^^^^^^^^^^^^^^^^
  Error: The spliced code assigns z (its context has z : mut ...), so it must be provided by a variable bound by [let mut] or a mutable toplevel
  File "a.stag", line 14, characters 40-47:
  14 | let annotated : <unit>{z : immut int} = assigns
                                               ^^^^^^^
  Error: The row {z : mut int; ..'_a} is incompatible with the row {z : immut int}
  The code of one assigns z, which the other requires it not to.
  [1]
