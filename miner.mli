(* The type of a mining instance *)
type t

(* [create pull_stream push_stream] is a new mining instance which can fetch
 * new candidate block headers from [pull_stream] and push solved blocks back 
 * to [push_stream]. *)
val create : Block.header Lwt_stream.t -> (Block.header option -> unit) -> t

(* [start miner cores] starts the mining worker [miner] running on
 * [cores] CPU cores. *)
val start : t -> int -> unit

(* [stop miner] halts [miner]. [miner] can be restarted with [start]. *)
val stop : t -> unit
