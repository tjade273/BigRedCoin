(* The header of a block in the chain. Version is the version of the protocol.
 * The prev_hash is the hash of the previous block. The merkle_root is the
 * root of this block in the merkle_tree. The nonce is as in the proof of work
 * algorithm. nBits is a compressed version of the mining difficulty. The 
 * timestamp is the seconds since the unix epoch when the block was made. *)
type header = {
  version : int;
  prev_hash : string;
  merkle_root : string;
  nonce : int;
  nBits : int;
  timestamp : int
}

(* A block consists of its header, the transactions in the block, and a count
 * of the number of transactions in the block. *)
type t = {
  header : header;
  transactions : Transaction.t list;
  transactions_count : int
}

let serialize = failwith "Unimplemented"
let deserialize = failwith "Unimplemented"
let hash = failwith "Unimplemented"

(* [target nbits] is the decompressed target that [nbits] represents. *)
let target = failwith "Unimplemented"

(* [difficulty nbits] is the "reciprocal" of the target, a measure of the 
 * proportion of the maximum possible difficulty. *)
let difficulty = failwith "Unimplemented"

(* [next_target header] is the compressed form of the target for the next block,
 * given the timestamp and difficulty of the previous block. *)
let next_difficulty = failwith "Unimplemented"
