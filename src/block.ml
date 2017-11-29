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

let target nbits =
  if (nbits < 0) then 
    "0x000000000000000000000000000000000000000000000000"
  else 
    let significant = Printf.sprintf "%X" (nbits mod 24) in
    let zeros = String.make (nbits/24) '0'
    "0x" ^ significant ^ zeros

(* [difficulty nbits] is the "reciprocal" of the target, a measure of the 
 * proportion of the maximum possible difficulty. *)
val difficulty : int -> int

(* [next_target header] is the compressed form of the target for the next block,
 * given the timestamp and difficulty of the previous block. *)
val next_difficulty : header -> int
