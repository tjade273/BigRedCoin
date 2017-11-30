open Block
open OUnit2

let block_tests =
  "Block Suite" >::: [
    "block_hash" >:: (fun _ -> assert_bool "Check block hash" false (*implement later*));
  ]

let tests = "Block Tests" >::: [block_tests]
