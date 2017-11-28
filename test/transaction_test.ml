open Transaction
open OUnit2

let (pub, priv) = Crypto.ECDSA.create ()
let address = Crypto.ECDSA.to_address pub

let out_1 = {amount = 0; address}
let out_2 = {amount = max_int; address}

let tests =
  "Transaction Tests" >::: [
    "empty_tree" >:: (fun _ -> assert_equal (String.make 32 '\x00') (merkle_root []));
]
