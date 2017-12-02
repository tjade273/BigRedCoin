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
  let t = target_block_time*blocks_per_recalculation in
  let next_t = (head.nBits * 100 *
               (head.timestamp - prev.timestamp)/(10000 * t)) in
  if next_t/head.nBits < 4 then next_t else 4*next_t

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
  let head = {
    version = header.version;
    prev_hash = Bytes.to_string header.prev_hash;
    merkle_root = Bytes.to_string header.merkle_root;
    nonce = header.nonce;
    nBits = header.n_bits;
    timestamp = header.timestamp
  } in
  let trans = List.map Transaction.demessageify txs in
  {header = head; transactions = trans; transactions_count = tx_count}
