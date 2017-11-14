

(* [pow t b] is true if the SHA-256 hash of the input block [b] is less than or
 * equal to the target [t], and false otherwise. This indicates a sucessful 
 * proof of work. *)
val pow : int -> Block.block -> bool
