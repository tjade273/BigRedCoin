type t

(* [create dir] is an instance of the full blockchain tree backed
 * by the LevelDB database in directory [dir]. If no database exists at
 * [dir] an new one is initialized. *)
val create : string -> t Lwt.t

(* [best_chain b] is a longest chain of [b]. If two chains are equally long,
 * it is unspecified which will be returned. *)

(* [insert_block chain block] inserts [block] into the leveldb database and adds
 * it to the chain specified in [block.prevblock] if such a block exists in the
 * database. Returns [true] on success. If the parent is unknown, return [false].
 * *)
val insert_block : t -> Block.block -> bool Lwt.t
