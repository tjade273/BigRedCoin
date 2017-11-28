open OUnit2

let tests = "Big Red Coin Tests" >::: [Crypto_test.tests; Database_test.tests]

let () = run_test_tt_main tests
