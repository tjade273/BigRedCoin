open OUnit2

let tests = "Big Red Coin Tests" >::: [Crypto_test.tests; Transaction_test.tests; Repl_test.tests]
let lwt_tests = Database_test.tests @ P2ptest.tests
let () = run_test_tt_main tests; Lwt_test.run "all_test" lwt_tests
