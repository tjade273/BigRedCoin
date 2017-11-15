(* The type of a JSON-RPC server *)
type t

(* [create port blockchain] is an RPC server listening on localhost:port
 * with [blockchain] as a backend. *)
val create : int -> Blockchain.t -> t

(* [stop t] unbinds [t] from its port and stops the server *)
val stop : t -> unit

