(* The type of a miner *)
type t

(* [write w s] writes the length of the string [s] followed by [s] to [w]. *)
val write : Lwt_io.output_channel -> string -> unit Lwt.t

(* [read r] is the next message in [r]. *)
val read : Lwt_io.input_channel -> string Lwt.t

(* [create addr f chain] is a new mining instance for the blockchain [chain]
 * from address [addr] and the stream [f] to push new blocks to. *)
val create : string -> (Block.t option -> unit) -> Blockchain.t ref -> t

(* [start miner] starts the mining worker [miner]. *)
val start : t -> unit Lwt.t

(* [stop miner] halts [miner]. [miner] can be restarted with [start]. *)
val stop : t -> unit
