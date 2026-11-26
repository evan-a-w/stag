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

Reading files is allowed at compile time, eg. to embed them; acting on the
world otherwise isn't, so checking is reproducible.

  $ cat > compile_time.stag <<'EOF'
  > let length = .~(let n = String.length (read_file "out.txt") in .< n >.)
  > let main = print_int length
  > EOF
  $ stag run compile_time.stag
  8
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
