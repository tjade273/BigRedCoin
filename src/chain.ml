type t = {head: Block.block; height: int}

let head {head; _} = head

let height : t -> int

let block_at_index

let tx_confirmations : t ->  string -> int

let insert_block : t -> Block.block -> t
