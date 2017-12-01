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
  transactions : Transaction.transaction list;
  transactions_count : int
}

type t = block

(* Based on the bitcoin developer reference. *)
let target nbits =
  if (nbits < 0) then 
    "0x0"
  else 
    let significant = Printf.sprintf "%X" (nbits mod 24) in
    let zeros = String.make (nbits/24) '0' in
    "0x" ^ significant ^ zeros

(* Based on the algorithm from the bitcoin wiki. *)
let difficulty nbits = 
  let b = log10 (float_of_int 0x00ffff) in
  let s = log10 256.0 in
  let exp = (b -. (log10 (float_of_int (0x00ffffff land nbits))) 
            +. s *. float_of_int (0x1d - ((nbits land 0xff000000) lsr 24))) in
  int_of_float (10**exp)

(* Based on the bitcoin difficulty update scheme. *)
let next_difficulty head =
  let t = float_of_int (target_block_time*blocks_per_recalculation) in
  let f_nBits = float_of_int head.nBits in
  let next_t = f_nBits *. (Unix.time () -. float_of_int head.timestamp)/.t in
  if next_t < 4.0 then int_of_float next_t else 4

let hash b =
  let head = b.header in
  let v = string_of_int head.version in
  let n = string_of_int head.nonce in
  let b = string_of_int head.nBits in
  let t = string_of_int head.nBits in
  let s = String.concat ";" [v; head.prev_hash; head.merkle_root; n; b; t] in
  Crypto.sha256 (Crypto.sha256 s)
  
