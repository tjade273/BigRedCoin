module BlockDB : Database.S with type value = Block.t

(* The type of a single chain of valid blocks. Recent blocks are cached,
 * while an on-disk DB backs rare long-range operations. *)
type t

(* [create db block] is the chain with a single block [block]
 * backed by the database [db] *)
val create : BlockDB.t -> Block.t -> t

(* [head chain] is the current highest block in the [chain]. *)
val head : t -> Block.t

(* [height chain] is the number of blocks since the genesis in [chain]. *)
val height : t -> int

(* [hash chain] is the hash of the highest block in [chain] *)
val hash : t -> string

(* [serialize chain] is a serialized representation of [chain], contains the
 * height and hash of the head block of [chain]. *)
val serialize : t -> string

(* [deserialize db s] is the chain obtained from deserializing [s] and obtaining
 * the head block from [db]. The deserialized chain is extende to a cache of size
 * 25. *)
val deserialize : BlockDB.t -> string -> t Lwt.t

(* [block_at_index chain i] is the [i]th block in [chain].
 * Requires: [i <= height chain] *)
val block_at_index : t -> int -> Block.t Lwt.t

(* [tx_confirmations txid] is the number of blocks in [chain] mined on
 * top of the block in which [txid] is included. [0] if there [txid] is not in
 * the chain. *)
(* val tx_confirmations : t ->  string -> int *)

(* [extend chain new_block] is [chain] extended by one block if [new_block] is a
 * valid head. [chain] is unmodified if [new_block] is not a valid successor. *)
val extend : t -> Block.block -> t option Lwt.t

(* [revert c h] is [(blocks, chain)] where [chain] is [c] reverted back to the
 * block with hash [h], and [blocks] is the list of blocks that were removed
 * as part of the reverting. *)
val revert : t -> string -> Block.t list * t

(* [next_difficulty chain] is the difficulty for the miners for the next block
 * to be added to [chain]. The difficulty is recomputed every 2016 blocks. *)
val next_difficulty : t -> int Lwt.t
