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

let header3 = {
  header1 with
  timestamp = header1.timestamp + 30*2016
}

let header4 = {
  header1 with
  timestamp = header1.timestamp + 30*2016*5
}

let header5 = {
  header1 with
  timestamp = header1.timestamp + 2*2016
}

let header6 = {
  header1 with
  timestamp = header1.timestamp + (30*2016*3)/2
}

let header7 = {
  header1 with
  timestamp = header1.timestamp;
  nBits = 0x18fbc330
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
      "nbits" >:: (fun _ -> assert_equal (Hex.to_string (`Hex((String.make 16 '0') ^ "1bc330" ^ (String.make 42 '0')))) (target 0x181bc330));
      "difficulty_adjust_1" >:: (fun _ -> assert_equal header3.nBits (next_difficulty header3 header1));
      "difficulty_adjust_1/4" >:: (fun _ -> assert_equal 0x1806f0cc (next_difficulty header4 header1));
      "difficulty_adjust_4" >:: (fun _ -> assert_equal 0x186f0cc4 (next_difficulty header5 header1));
      "difficulty_adjust_2/3" >:: (fun _ -> assert_equal 0x18128220 (next_difficulty header6 header1));
      "difficulty_wrap" >:: (fun _ -> assert_equal 0x1903ef0c (next_difficulty header7 header1));
    "block_hash" >:: (fun _ -> assert_equal ~msg:"Check block hash" (Hex.to_string (`Hex "5b4f5c17f4337bcf1bdcea9cd639813ab263d54f9cc8eace675059ed31d042ff")) (hash block1));
    "serialize/deserialize" >:: (fun _ -> assert_equal block1 (deserialize (serialize block1)));
  ]

let tests = "Block Tests" >::: [block_tests]
