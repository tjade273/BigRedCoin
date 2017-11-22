open Crypto
open OUnit2

let hex_cmp hex bytes =
  `Hex hex = Hex.of_string bytes

let (pubkey, privkey) = ECDSA.create ()

let tests =
  "Crypto Tests" >::: [
    "sha256" >:: (fun _ -> assert_equal ~cmp:hex_cmp "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9" (Crypto.sha256 "hello world"));
    "ecdsa_sign_msg" >:: (fun _ -> assert_bool "Verify signature" (ECDSA.verify (ECDSA.to_address pubkey) "hello world" (ECDSA.sign (pubkey,privkey) "hello world")));
    "ecdsa_fake_msg" >:: (fun _ -> assert_bool "Fake signature"  @@ not (ECDSA.verify (ECDSA.to_address pubkey) "hello world" (ECDSA.sign (pubkey,privkey) "it's a trap!")));
  ]

let () = run_test_tt_main tests

