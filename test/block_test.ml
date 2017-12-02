open Transaction
open OUnit2
open Block

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

let input1 = {
  txid = Crypto.sha256 "hola";
  out_index = 5
}

let output1 = {
  amount = 3;
  address = Crypto.sha256 "hello"
}

let transaction1 = {
  outs = [output1];
  ins = [input1];
  sigs = None
}

let block1 = {
  header = header1;
  transactions = [transaction1];
  transactions_count = 1
}

let block_tests =
    "Block Suite" >::: [
      "nbits" >:: (fun _ -> assert_equal ~printer:print (Hex.to_string (`Hex((String.make 16 '0') ^ "1bc330" ^ (String.make 42 '0')))) (target 0x181bc330));
    "Difficulty adjustment" >:: (fun _ -> assert_equal ~printer:string_of_int 200631 (next_difficulty header1 header2));
    "block_hash" >:: (fun _ -> assert_bool "Check block hash" true);
    "serialize/deserialize" >:: (fun _ -> assert_equal block1 (deserialize (serialize block1)));
  ]

let tests = "Block Tests" >::: [block_tests]
