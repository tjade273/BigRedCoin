(* [get_block hash] sends an RPC command to get the block with hash [hash] *)
val get_block : string -> Block.block Lwt.t

(* [send account amount address] sends an RPC command to have [account] send
 * [amount] coins to [address]. *)
val send : Account.t -> int -> string Lwt.t

(* [balance address] sends an RPC command to check the current balance of
 * [address]. *)
val balance : string -> int Lwt.t
