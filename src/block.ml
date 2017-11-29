let TARGET_BLOCK_TIME = 30
let BLOCKS_PER_RECALCULATION = 2016

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

(* Based on the bitcoin developer reference. *)
let target nbits =
  if (nbits < 0) then 
    "0x0"
  else 
    let significant = Printf.sprintf "%X" (nbits mod 24) in
    let zeros = String.make (nbits/24) '0'
    "0x" ^ significant ^ zeros

(* Based on the algorithm from the bitcoin wiki. *)
let difficulty nbits = 
  let b = log10 (0x00ffff) in
  let s = log10 256.0 in
  10.0**(b-.log10 (land 0x00ffffff nbits) + 
         .s*.(float_of_int (0x1d-.(lsr (land nbits 0xff000000) 24))))

(* Based on the bitcoin difficulty update scheme. *)
let next_difficulty head =
  let t = float_of_int (TARGET_BLOCK_TIME*BLOCKS_PER_RECALCULATION)
  let f_nBits = float_of_int head.nbits in
  let next_t = f_nBits *. (float_of_int (Unix.time - head.timestamp)/.t in
  if next_t < 4 then next_t else 4

let blockhash head =
  let v = string_of_int head.version in
  let n = string_of_int head.nonce in
  let b = string_of_int head.nBits in
  let t = string_of_int head.nBits in
  let s = String.concat ";" [v; head.prev_hash; head.merkle_root; n; b; t] in
  Crypto.sha256 s
