type output = {
  amount: int;
  address : string
}

type input = {
  txid : string;
  out_index : int
}

type t = {
  outs : output list;
  ins : input list;
  sigs : string list option
}

let messageify {outs; ins; _} =
  let outs = List.map (
      fun {amount; address} ->
        Message_types.({address = Bytes.of_string address; amount})
    ) outs
  in
  let ins = List.map (
      fun {txid; out_index} ->
        Message_types.({txid = Bytes.of_string txid; out_index})
    ) ins
  in
  Message_types.({outs; ins; sigs=[]})

let serialize t =
  let tx = messageify t in
  let encoder = Pbrt.Encoder.create () in
  Message_pb.encode_transaction tx encoder;
  Pbrt.Encoder.to_bytes encoder

let demessageify {Message_types.outs; ins; _} =
  let outs = List.map (
      fun {Message_types.amount; address} ->
        ({address = Bytes.to_string address; amount})
    ) outs
  in
  let ins = List.map (
      fun {Message_types.txid; out_index} ->
        ({txid = Bytes.to_string txid; out_index})
    ) ins
  in
  {outs; ins; sigs = None}

let deserialize s =
  let decoder = Pbrt.Decoder.of_bytes s in
  demessageify (Message_pb.decode_transaction decoder)

let hash tx =
  let s = serialize tx in
  Crypto.sha256 s

let signers ({outs; ins; sigs} as t) =
  match sigs with
  | None -> None
  | Some s ->
  try
    let force f = function Some x -> f x | None -> invalid_arg "None" in
    let msg = serialize t in
    let rec_sigs = List.map (Crypto.ECDSA.sig_of_string) s in
    let addrs = List.map (force (Crypto.ECDSA.recover msg)) rec_sigs in
    Some (List.map (force Crypto.ECDSA.to_address) addrs)
  with _ -> None

let merkle_root txs =
  let tx_hashes = List.map hash txs in
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
