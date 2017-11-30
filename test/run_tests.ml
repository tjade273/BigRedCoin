open OUnit2

let tests = "Big Red Coin Tests" >::: [Crypto_test.tests]
let lwt_tests = Database_test.tests @ P2ptest.tests
let () = run_test_tt_main tests
let () = Lwt_test.run "all_test" lwt_tests