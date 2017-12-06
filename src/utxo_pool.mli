type t 
exception Invalid_block

val apply : Block.t -> t   

val revert : Block.t -> t 

