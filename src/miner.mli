(* The type of a mining instance *)
type t

(* [write w s] writes the length of the string [s] followed by [s] to [w]. *)
val write : Lwt_io.output_channel -> string -> unit Lwt.t

(* [read r] is the next message in [r]. *)
val read : Lwt_io.input_channel -> string Lwt.t

(* [create pull_stream push_stream] is a new mining instance which can fetch
 * new candidate block headers from [pull_stream] and push solved blocks back 
 * to [push_stream]. *)
val create : string -> (Block.t option -> unit) -> Blockchain.t ref -> t

(* [start miner] starts the mining worker [miner]. *)
val start : t -> unit Lwt.t

(* [stop miner] halts [miner]. [miner] can be restarted with [start]. *)
val stop : t -> unit
