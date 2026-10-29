(rule
 (targets
  stag_ast.src.tests.rs.corrected
  stag_ast_example.src.tests.rs.corrected
 )
 (deps
  expect_tests.sh
  (source_tree .))
 (action
  (run bash expect_tests.sh
   stag_ast/src/tests.rs
   stag_ast_example/src/tests.rs
  )))

(rule
 (alias runtest)
 (action
  (diff stag_ast/src/tests.rs stag_ast.src.tests.rs.corrected)))

(rule
 (alias runtest)
 (action
  (diff stag_ast_example/src/tests.rs stag_ast_example.src.tests.rs.corrected)))
