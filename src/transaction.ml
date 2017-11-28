type output = {
  amount: int;
  address : string
}


type input = {
  txid : string;
  out_index : int;
}

type transaction = {
  outs : output list;
  ins : input list;
  sigs : string list;
}

let serialize_output {amount; address} =
  let buffer = Cstruct.create 28 in
  Cstruct.LE.set_uint64 buffer 0 (Int64.of_int amount);
  Cstruct.blit_from_string address 0 buffer 8 20;
  Cstruct.to_string buffer

let serialize_input {txid; out_index} =
  let buffer = Cstruct.create 36 in
  Cstruct.blit_from_string txid 0 buffer 0 32;
  Cstruct.LE.set_uint32 buffer 32 (Int32.of_int out_index);
  Cstruct.to_string buffer

let serialize_transaction {outs; ins; _} =
  let out_s = List.map serialize_output outs in
  let in_s = List.map serialize_input ins in
  List.fold_left (^) "" (out_s @ in_s)

let txid tx =
  let s = serialize_transaction tx in
  Crypto.sha256 s

let signers ({outs; ins; sigs} as t) =
  try
    let force f = function Some x -> f x | None -> invalid_arg "None" in
    let msg = serialize_transaction t in
    let sigs = List.map (Crypto.ECDSA.sig_of_string) sigs in
    let addrs = List.map (force (Crypto.ECDSA.recover msg)) sigs in
    Some (List.map (force Crypto.ECDSA.to_address) addrs)
  with _ -> None

let merkle_root txs =
  let sha tx = Crypto.sha256 (serialize_transaction tx) in
  let tx_hashes = List.map sha txs in
  let rec pair = function
    | [] -> []
    | x :: [] -> Crypto.sha256(x^x)::[]
    | x::y::tl -> Crypto.sha256(x^y)::pair(tl)
  in
  let rec hash_tree = function
    | [] -> String.make 32 '\x00'
    | x::[] -> x
    | l -> hash_tree (pair l)
  in
  hash_tree tx_hashes
