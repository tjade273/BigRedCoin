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
  if (nbits < 0) then 
    String.make 32 '\x00'
  else 
    let significant = Printf.sprintf "%X" (nbits mod 24) in
    let zeros = String.make (nbits/24) '0' in
    Hex.to_string (`Hex (significant ^ zeros))

(* Based on the algorithm from the bitcoin wiki. *)
let difficulty nbits = 
  let b = log10 (float_of_int 0x00ffff) in
  let s = log10 256.0 in
  let exp = (b -. (log10 (float_of_int (0x00ffffff land nbits))) 
            +. s *. float_of_int (0x1d - ((nbits land 0xff000000) lsr 24))) in
  int_of_float (10.0**exp)

(* Based on the bitcoin difficulty update scheme. *)
let next_difficulty head =
  let t = float_of_int (target_block_time*blocks_per_recalculation) in
  let f_nBits = float_of_int head.nBits in
  let next_t = f_nBits *. (Unix.time () -. float_of_int head.timestamp)/.t in
  if next_t < 4.0 then int_of_float next_t else 4

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
