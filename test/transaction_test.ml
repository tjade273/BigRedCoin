open Transaction
open OUnit2

let (pub, priv) = Crypto.ECDSA.create ()
let address = Crypto.ECDSA.to_address pub

let out_1 = {amount = 0; address}
let out_2 = {amount = max_int; address}

let in_1 = {txid = String.make 32 '\x00'; out_index = 0}
let in_2 = {txid = String.make 32 '\x00'; out_index = 1}

let tx_1 = {outs = [out_1]; ins = [in_1; in_2]; sigs = None}

let sig_1 = Crypto.ECDSA.sign (pub,priv) (serialize_transaction tx_1)
            |> Crypto.ECDSA.string_of_sig

let addr_1 = Crypto.ECDSA.to_address pub

let tests =
  "Transaction Tests" >::: [
    "empty_tree" >:: (fun _ -> assert_equal (String.make 32 '\x00') (merkle_root []));
    "signers" >:: (fun _ -> assert_equal (Some [addr_1]) (signers {tx_1 with sigs = Some [sig_1]}));
    "invalid_sig" >:: (fun _ -> assert_equal None (signers {tx_1 with sigs = Some ["lalala"^sig_1]}));
]
