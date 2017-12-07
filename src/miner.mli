(* The type of a mining instance *)
type t

(* [equiv block1 block2] is true if [block1] equals [block2] in all fields but
 * the header's nonce and timestamp. 
 * requires: block1 and block2 both have a nonempty transaction. *)
val equiv : Block.t -> Block.t -> bool 

(* [create pull_stream push_stream] is a new mining instance which can fetch
 * new candidate block headers from [pull_stream] and push solved blocks back 
 * to [push_stream]. *)
val create : string -> (Block.t option -> unit) -> Blockchain.t ref -> t

(* [start miner] starts the mining worker [miner]. *)
val start : t -> unit Lwt.t

(* [stop miner] halts [miner]. [miner] can be restarted with [start]. *)
val stop : t -> unit
