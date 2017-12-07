(* The type of a miner *)
type t

(* [equiv block1 block2] is true if [block1] equals [block2] in all fields but
 * the header's nonce and timestamp. 
 * requires: block1 and block2 both have a nonempty transaction. *)
val equiv : Block.t -> Block.t -> bool 

(* [create addr f chain] is a new mining instance for the blockchain [chain]
 * from address [addr] and the stream [f] to push new blocks to. *)
val create : string -> (Block.t option -> unit) -> Blockchain.t ref -> t

(* [start miner] starts the mining worker [miner]. *)
val start : t -> unit Lwt.t

(* [stop miner] halts [miner]. [miner] can be restarted with [start]. *)
val stop : t -> unit
