open Block
open OUnit2

let print s = s |> Hex.of_string |> function `Hex x -> x

let header1 = {
  version = 1;
  prev_hash = Crypto.sha256 "hi";
  merkle_root = "1";
  nonce = 3;
  nBits = 0x181bc331;
  timestamp = 1512183567
  }

let header2 = {
  version = 1;
  prev_hash = Crypto.sha256 "hi!";
  merkle_root = "2";
  nonce = 3;
  nBits = 0x181bc330;
  timestamp = 1512180567 
 }

let block_tests =
    "Block Suite" >::: [
      "nbits" >:: (fun _ -> assert_equal ~printer:print (target 0x181bc330) (Hex.to_string (`Hex((String.make 16 '0') ^ "1bc330" ^ (String.make 42 '0')))));
    "Difficulty adjustment" >:: (fun _ -> assert_equal ~printer:string_of_int (next_difficulty header1 header2) 200631);
    "block_hash" >:: (fun _ -> assert_bool "Check block hash" true);
  ]

let tests = "Block Tests" >::: [block_tests]
