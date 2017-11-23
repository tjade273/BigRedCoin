[@@@ocaml.warning "-27-30-39"]


type get_request =
  | Peer 
  | Mempool 
  | Blocks 

type get = {
  request : get_request;
  startblock : bytes option;
}

type transaction_output = {
  amount : int64;
  address : bytes;
}

type transaction_input = {
  txid : bytes;
  out_index : int32;
  signature : bytes;
}

type transaction = {
  outs : transaction_output list;
  ins : transaction_input list;
}

type block_header = {
  version : int32;
  prev_hash : bytes;
  merkle_root : bytes;
  nonce : int64;
  n_bits : int64;
  timestamp : int64;
}

type block = {
  header : block_header;
  txs : transaction list;
  tx_count : int32;
}

type peer = {
  address : string;
  port : int;
  last_seen : int;
}

type manage_manage_t =
  | Ping 
  | Pong 
  | Peer_p 
  | Data_p 

type manage = {
  manage_type : manage_manage_t;
  peers : peer list;
}

type post = {
  transactions : transaction list;
  blocks : block list;
}

type message_method =
  | Get 
  | Post 
  | Manage 

type message = {
  method_ : message_method;
  get : get option;
  post : post option;
  manage : manage option;
}

let rec default_get_request () = (Peer:get_request)

let rec default_get 
  ?request:((request:get_request) = default_get_request ())
  ?startblock:((startblock:bytes option) = None)
  () : get  = {
  request;
  startblock;
}

let rec default_transaction_output 
  ?amount:((amount:int64) = 0L)
  ?address:((address:bytes) = Bytes.create 0)
  () : transaction_output  = {
  amount;
  address;
}

let rec default_transaction_input 
  ?txid:((txid:bytes) = Bytes.create 0)
  ?out_index:((out_index:int32) = 0l)
  ?signature:((signature:bytes) = Bytes.create 0)
  () : transaction_input  = {
  txid;
  out_index;
  signature;
}

let rec default_transaction 
  ?outs:((outs:transaction_output list) = [])
  ?ins:((ins:transaction_input list) = [])
  () : transaction  = {
  outs;
  ins;
}

let rec default_block_header 
  ?version:((version:int32) = 0l)
  ?prev_hash:((prev_hash:bytes) = Bytes.create 0)
  ?merkle_root:((merkle_root:bytes) = Bytes.create 0)
  ?nonce:((nonce:int64) = 0L)
  ?n_bits:((n_bits:int64) = 0L)
  ?timestamp:((timestamp:int64) = 0L)
  () : block_header  = {
  version;
  prev_hash;
  merkle_root;
  nonce;
  n_bits;
  timestamp;
}

let rec default_block 
  ?header:((header:block_header) = default_block_header ())
  ?txs:((txs:transaction list) = [])
  ?tx_count:((tx_count:int32) = 0l)
  () : block  = {
  header;
  txs;
  tx_count;
}

let rec default_peer 
  ?address:((address:string) = "")
  ?port:((port:int) = 0)
  ?last_seen:((last_seen:int) = 0)
  () : peer  = {
  address;
  port;
  last_seen;
}

let rec default_manage_manage_t () = (Ping:manage_manage_t)

let rec default_manage 
  ?manage_type:((manage_type:manage_manage_t) = default_manage_manage_t ())
  ?peers:((peers:peer list) = [])
  () : manage  = {
  manage_type;
  peers;
}

let rec default_post 
  ?transactions:((transactions:transaction list) = [])
  ?blocks:((blocks:block list) = [])
  () : post  = {
  transactions;
  blocks;
}

let rec default_message_method () = (Get:message_method)

let rec default_message 
  ?method_:((method_:message_method) = default_message_method ())
  ?get:((get:get option) = None)
  ?post:((post:post option) = None)
  ?manage:((manage:manage option) = None)
  () : message  = {
  method_;
  get;
  post;
  manage;
}
