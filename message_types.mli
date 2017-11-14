(** message.proto Types *)



(** {2 Types} *)

type get_request =
  | Ping
  | Pong
  | Peer
  | Mempool
  | Blocks

type get = {
  request : get_request;
  startblock : bytes option;
}

type transaction_output = {
  amount : int;
  address : bytes;
}

type transaction_input = {
  txid : bytes;
  out_index : int;
  signature : bytes;
}

type transaction = {
  outs : transaction_output list;
  ins : transaction_input list;
}

type block_header = {
  version : int;
  prev_hash : bytes;
  merkle_root : bytes;
  nonce : int;
  n_bits : int;
  timestamp : int;
}

type block = {
  header : block_header;
  txs : transaction list;
  tx_count : int;
}

type peer = {
  address : string;
  port : int;
  last_seen : int;
}

type post = {
  transactions : transaction list;
  blocks : block list;
  peers : peer list;
}


(** {2 Default values} *)

val default_get_request : unit -> get_request
(** [default_get_request ()] is the default value for type [get_request] *)

val default_get :
  ?request:get_request ->
  ?startblock:bytes option ->
  unit ->
  get
(** [default_get ()] is the default value for type [get] *)

val default_transaction_output :
  ?amount:int ->
  ?address:bytes ->
  unit ->
  transaction_output
(** [default_transaction_output ()] is the default value for type [transaction_output] *)

val default_transaction_input :
  ?txid:bytes ->
  ?out_index:int ->
  ?signature:bytes ->
  unit ->
  transaction_input
(** [default_transaction_input ()] is the default value for type [transaction_input] *)

val default_transaction :
  ?outs:transaction_output list ->
  ?ins:transaction_input list ->
  unit ->
  transaction
(** [default_transaction ()] is the default value for type [transaction] *)

val default_block_header :
  ?version:int ->
  ?prev_hash:bytes ->
  ?merkle_root:bytes ->
  ?nonce:int ->
  ?n_bits:int ->
  ?timestamp:int ->
  unit ->
  block_header
(** [default_block_header ()] is the default value for type [block_header] *)

val default_block :
  ?header:block_header ->
  ?txs:transaction list ->
  ?tx_count:int ->
  unit ->
  block
(** [default_block ()] is the default value for type [block] *)

val default_peer :
  ?address:string ->
  ?port:int ->
  ?last_seen:int ->
  unit ->
  peer
(** [default_peer ()] is the default value for type [peer] *)

val default_post :
  ?transactions:transaction list ->
  ?blocks:block list ->
  ?peers:peer list ->
  unit ->
  post
(** [default_post ()] is the default value for type [post] *)
