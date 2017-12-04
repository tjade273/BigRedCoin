let target_block_time = 30
let blocks_per_recalculation = 2016

type header = {
  version : int;
  prev_hash : string;
  merkle_root : string;
  nonce : int;
  nBits : int;
  timestamp : int
}

type block = {
  header : header;
  transactions : Transaction.t list;
  transactions_count : int
}

type t = block

(* Based on the bitcoin developer reference. *)
let target nbits =
  if nbits < 0 then String.make 32 '\000'
  else
    let exponent = nbits lsr 24 in
    let nbit_buf = Cstruct.create 4 in
    let target_buf = Cstruct.create 32 in
    Cstruct.BE.set_uint32 nbit_buf 0 (Int32.of_int nbits);
    Cstruct.blit nbit_buf 1 target_buf (32 - exponent) 3;
    Cstruct.to_string target_buf

(* Based on the algorithm from the bitcoin wiki. *)
let difficulty nbits =
  let b = log10 (float_of_int 0x00ffff) in
  let s = log10 256.0 in
  let exp = (b -. (log10 (float_of_int (0x00ffffff land nbits)))
            +. s *. float_of_int (0x1d - ((nbits land 0xff000000) lsr 24))) in
  int_of_float (10.0**exp)

(* Based on the bitcoin difficulty update scheme. *)
let next_difficulty head prev =
  let rec normalize n =
    if n > 0xFFFFFF then
      let n', e' = (normalize (n/256)) in
      (n', e'+1)
    else
      (n, 0)
  in
  let adjust n expected actual =
      let mantissa = n land 0xFFFFFF in
      let exponent = n lsr 24 in
      let m', e' = normalize @@ (mantissa * expected)/actual in
      ((exponent + e') lsl 24) land m'
  in
  let expected_time = target_block_time*blocks_per_recalculation in
  let actual_time = head.timestamp - prev.timestamp in
  if expected_time / (max actual_time 1) > 4 then
    adjust head.nBits 4 1
  else if actual_time / expected_time > 4 then
    adjust head.nBits 1 4
  else
    adjust head.nBits expected_time actual_time

(* [messageify_header h] is the protobuf encoded message representing [h]. *)
let messageify_header {version;
                       prev_hash;
                       merkle_root;
                       nonce;
                       nBits;
                       timestamp} =
  Message_types.({
    Message_types.version = version;
    prev_hash = Bytes.of_string prev_hash;
    merkle_root = Bytes.of_string merkle_root;
    nonce = nonce;
    n_bits = nBits;
    timestamp = timestamp
  }) 

let hash b =
  let h = messageify_header b.header in
  let encoder = Pbrt.Encoder.create () in
  Message_pb.encode_block_header h encoder;
  let s = Pbrt.Encoder.to_bytes encoder in
  Crypto.sha256 (Crypto.sha256 s)

let serialize b =
  let header_ser = messageify_header b.header in
  let block_ser = Message_types.({ 
    header = header_ser;
    txs = List.map Transaction.messageify b.transactions;
    tx_count = b.transactions_count
  }) in
  let encoder = Pbrt.Encoder.create () in
  Message_pb.encode_block block_ser encoder;
  Pbrt.Encoder.to_bytes encoder

let deserialize s =
  let decoder = Pbrt.Decoder.of_bytes s in
  let Message_types.({header; txs; tx_count}) = 
    Message_pb.decode_block decoder in
  let head = Message_types.({
    version = header.version;
    prev_hash = Bytes.to_string header.prev_hash;
    merkle_root = Bytes.to_string header.merkle_root;
    nonce = header.nonce;
    nBits = header.n_bits;
    timestamp = header.timestamp
  }) in
  let trans = List.map Transaction.demessageify txs in
  {header = head; transactions = trans; transactions_count = tx_count}
