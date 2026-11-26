Programs read their arguments, after [--], with [argv ()].

  $ cat > args.stag <<'EOF'
  > let main = print_endline (String.concat "," (argv ()))
  > EOF
  $ stag run args.stag -- a "b c" -flag
  a,b c,-flag
  $ stag run args.stag
  

Files are read and written whole, raising [Sys_error] on failure.

  $ cat > files.stag <<'EOF'
  > let main =
  >   write_file "out.txt" "one\ntwo\n";
  >   print_string (read_file "out.txt");
  >   match read_file "missing.txt" with
  >   | _ -> ()
  > EOF
  $ stag run files.stag
  one
  two
  File "files.stag", line 4, characters 8-31:
  4 |   match read_file "missing.txt" with
              ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Sys_error "missing.txt: No such file or directory"
  [1]
  $ cat out.txt
  one
  two

  $ cat > unwritable.stag <<'EOF'
  > let main = write_file "no/such/dir.txt" ""
  > EOF
  $ stag run unwritable.stag
  File "unwritable.stag", line 1, characters 11-42:
  1 | let main = write_file "no/such/dir.txt" ""
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Sys_error "no/such/dir.txt: No such file or directory"
  [1]

stdin is read a line at a time, without its newline, or all at once.

  $ cat > lines.stag <<'EOF'
  > let echo _ =
  >   match read_line () with
  >   | Some line -> print_endline (String.append "| " line); echo ()
  >   | None -> ()
  > let main = echo ()
  > EOF
  $ printf 'one\ntwo\nno newline' | stag run lines.stag
  | one
  | two
  | no newline
  $ stag run lines.stag < /dev/null

Only [\n] and [\r\n] end a line: a lone [\r], even ending the input, is kept.

  $ printf 'crlf\r\nmid\rdle\r\nend\r' | stag run lines.stag | od -c | sed 's/  */ /g'
  0000000 | c r l f \n | m i d \r d l e
  0000020 \n | e n d \r \n
  0000030
  $ printf '\n\r\n' | stag run lines.stag
  | 
  | 

  $ cat > all.stag <<'EOF'
  > let first = read_line ()
  > let rest = read_all_stdin ()
  > let main =
  >   print_endline (Core.Option.value first "");
  >   print_string (String.append "rest: " rest)
  > EOF
  $ printf 'a\nb\nc\n' | stag run all.stag
  a
  rest: b
  c

[prerr_string] and [prerr_endline] write to stderr, after flushing stdout, so
the two show in order.

  $ cat > err.stag <<'EOF'
  > let main =
  >   print_string "out ";
  >   prerr_endline "err";
  >   print_endline "out again"
  > EOF
  $ stag run err.stag 2>&1
  out err
  out again
  $ stag run err.stag 2>/dev/null
  out out again

[exit] ends the program with its code, after flushing its output.

  $ cat > exit.stag <<'EOF'
  > let main =
  >   print_string "unterminated";
  >   exit 3
  > let never = print_endline "not printed"
  > EOF
  $ stag run exit.stag
  unterminated
  [3]

Exit codes are 0..255, as the system keeps only 8 bits: others are errors, so
that eg. [exit 256] can't report success.

  $ for code in 0 255 '(-1)' 256 4611686018427387903; do
  >   echo "let main = exit $code" > code.stag
  >   stag run code.stag; echo "exit status $?"
  > done
  exit status 0
  exit status 255
  File "code.stag", line 1, characters 11-20:
  1 | let main = exit (-1)
                 ^^^^^^^^^
  Error: exit -1: the exit code must be in 0..255
  exit status 1
  File "code.stag", line 1, characters 11-19:
  1 | let main = exit 256
                 ^^^^^^^^
  Error: exit 256: the exit code must be in 0..255
  exit status 1
  File "code.stag", line 1, characters 11-35:
  1 | let main = exit 4611686018427387903
                 ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: exit 4611686018427387903: the exit code must be in 0..255
  exit status 1

An uncaught exception also flushes the output before its error.

  $ cat > raise.stag <<'EOF'
  > let main =
  >   print_string "before";
  >   raise (Failure "oops")
  > EOF
  $ stag run raise.stag 2>&1
  beforeFile "raise.stag", line 3, characters 2-24:
  3 |   raise (Failure "oops")
        ^^^^^^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Failure "oops"
  [1]

Output is flushed before every C call, so C's [exit], which doesn't flush
stag's buffers, loses none of it.

  $ cat > c_exit.stag <<'EOF'
  > external c_exit : C.int -> unit = "exit"
  > let main =
  >   print_string "before C's exit";
  >   c_exit 4
  > EOF
  $ stag run c_exit.stag
  before C's exit
  [4]

Acting on the world at compile time, beyond printing, is an error, so checking
is reproducible. Even reading files: nothing would rebuild the program when
they change.

  $ cat > compile_time.stag <<'EOF'
  > let length = .~(let n = String.length (read_file "out.txt") in .< n >.)
  > EOF
  $ stag check compile_time.stag
  File "compile_time.stag", line 1, characters 13-71:
  1 | let length = .~(let n = String.length (read_file "out.txt") in .< n >.)
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This toplevel splice failed at compile time: read_file can't be called at compile time
  File "compile_time.stag", line 1, characters 38-59:
  1 | let length = .~(let n = String.length (read_file "out.txt") in .< n >.)
                                            ^^^^^^^^^^^^^^^^^^^^^
  Note: The error was raised here
  [1]
  $ cat > compile_time_write.stag <<'EOF'
  > let x = .~(write_file "z.txt" ""; .< 1 >.)
  > EOF
  $ stag check compile_time_write.stag
  File "compile_time_write.stag", line 1, characters 8-42:
  1 | let x = .~(write_file "z.txt" ""; .< 1 >.)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This toplevel splice failed at compile time: write_file can't be called at compile time
  File "compile_time_write.stag", line 1, characters 11-32:
  1 | let x = .~(write_file "z.txt" ""; .< 1 >.)
                 ^^^^^^^^^^^^^^^^^^^^^
  Note: The error was raised here
  [1]
  $ cat > compile_time_exit.stag <<'EOF'
  > let x = .~(exit 0)
  > EOF
  $ stag check compile_time_exit.stag
  File "compile_time_exit.stag", line 1, characters 8-18:
  1 | let x = .~(exit 0)
              ^^^^^^^^^^
  Error: This toplevel splice failed at compile time: exit can't be called at compile time
  File "compile_time_exit.stag", line 1, characters 10-18:
  1 | let x = .~(exit 0)
                ^^^^^^^^
  Note: The error was raised here
  [1]

Writing to stdout can fail, eg. when it is closed or the disk is full. A prim
whose write fails raises [Sys_error], as does the flush before a C call.

  $ cat > flush_fails.stag <<'EOF'
  > let main =
  >   print_string "x";
  >   match flush () with
  >   | () -> ()
  >   | exception Sys_error message -> exit 7
  > EOF
  $ stag run flush_fails.stag > /dev/full
  Error: Writing to stdout failed: No space left on device
  [7]
  $ cat > prerr_fails.stag <<'EOF'
  > let main = print_string "x"; prerr_endline "e"
  > EOF
  $ stag run prerr_fails.stag >&-
  File "prerr_fails.stag", line 1, characters 29-46:
  1 | let main = print_string "x"; prerr_endline "e"
                                   ^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Sys_error "Bad file descriptor"
  [1]
  $ stag run prerr_fails.stag > /dev/full
  File "prerr_fails.stag", line 1, characters 29-46:
  1 | let main = print_string "x"; prerr_endline "e"
                                   ^^^^^^^^^^^^^^^^^
  Error: Uncaught exception Sys_error "No space left on device"
  Error: Writing to stdout failed: No space left on device
  [1]
  $ cat > c_flush_fails.stag <<'EOF'
  > external labs : int -> int = "labs"
  > let main = print_string "x"; print_int (labs 1)
  > EOF
  $ stag run c_flush_fails.stag >&-
  File "c_flush_fails.stag", line 2, characters 39-47:
  2 | let main = print_string "x"; print_int (labs 1)
                                             ^^^^^^^^
  Error: Uncaught exception Sys_error "Bad file descriptor"
  [1]

Printing more than the buffer holds writes it, so may raise too.

  $ cat > print_fails.stag <<'EOF'
  > let double s n = match n with 0 -> s | _ -> double (String.append s s) (Int.sub n 1)
  > let big = double "x" 17
  > let main =
  >   match print_string big with
  >   | () -> ()
  >   | exception Sys_error message -> exit 8
  > EOF
  $ stag run print_fails.stag > /dev/full
  Error: Writing to stdout failed: No space left on device
  [8]

The output left when the program ends is flushed then, and a failure is an
error, whatever the exit code would have been.

  $ cat > final_flush.stag <<'EOF'
  > let main = print_string "x"
  > EOF
  $ stag run final_flush.stag >&-
  Error: Writing to stdout failed: Bad file descriptor
  [1]
  $ stag run final_flush.stag > /dev/full
  Error: Writing to stdout failed: No space left on device
  [1]
  $ stag run exit.stag > /dev/full
  Error: Writing to stdout failed: No space left on device
  [3]
