module TransactionDB : Database.S with type value = Transaction.t

type t
exception Invalid_block

val empty : TransactionDB.t -> t

val apply : t -> Block.t -> t Lwt.t

val revert : t -> Block.t -> t Lwt.t

val filter : t -> string -> (Transaction.input * Transaction.output) list

val verify : t -> Transaction.t -> unit

val force_transaction : t -> Transaction.t -> t