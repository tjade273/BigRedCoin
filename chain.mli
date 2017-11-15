(* The type of a single chain of valid blocks *)
type t

(* [head chain] is the hash of the current best block in the chain [t] *)
val head : t -> string

(* [height chain] is the number of blocks since the genesis in [chain] *)
val height : t -> int

(* [block_at_index chain i] is the [i]th block in [chain] *)
val block_at_index : t -> int -> Block.block

(* [tx_confirmations txid] is the number of blocks in [chain] mined on
 * top of the block in which [txid] is included. [0] if there [txid] is not in the chain*)
val tx_confirmations : t ->  string -> int

(* [insert_block chain block] is [chain] extended by one block if [block] is a valid
 * head. [chain] is unmodified if [block] is not a valid successor.*)
val insert_block : t -> Block.block -> t
