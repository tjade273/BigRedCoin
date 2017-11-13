module type Chain = sig
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
end

type t

(* [create dir] is an instance of the full blockchain tree backed
 * by the LevelDB database in directory [dir]. If no database exists at
 * [dir] an new one is initialized. *)
val create : string -> t Lwt.t

(* [best_chain b] is a longest chain of [b]. If two chains are equally long,
 * it is unspecified which will be returned *)
val best_chain : t -> Chain.t Lwt.t

(* [insert_block chain block] inserts [block] into the leveldb database and adds it to the chain
 * specified in [block.prevblock] if such a block exists in the database. Returns [true] on success.
 * If the parent is unknown, return [false]*)
val insert_block : t -> Block.block -> bool Lwt.t

(* [chain_of_hash db hash] is the chain with block [hash] as its head, if such a block exists.
 * None otherwise. *)
val chain_of_hash : t -> string -> Chain.t option Lwt.t
