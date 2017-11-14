[@@@ocaml.warning "-27-30-39"]


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

let rec default_get_request () = (Ping:get_request)

let rec default_get 
  ?request:((request:get_request) = default_get_request ())
  ?startblock:((startblock:bytes option) = None)
  () : get  = {
  request;
  startblock;
}

let rec default_transaction_output 
  ?amount:((amount:int) = 0)
  ?address:((address:bytes) = Bytes.create 0)
  () : transaction_output  = {
  amount;
  address;
}

let rec default_transaction_input 
  ?txid:((txid:bytes) = Bytes.create 0)
  ?out_index:((out_index:int) = 0)
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
  ?version:((version:int) = 0)
  ?prev_hash:((prev_hash:bytes) = Bytes.create 0)
  ?merkle_root:((merkle_root:bytes) = Bytes.create 0)
  ?nonce:((nonce:int) = 0)
  ?n_bits:((n_bits:int) = 0)
  ?timestamp:((timestamp:int) = 0)
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
  ?tx_count:((tx_count:int) = 0)
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

let rec default_post 
  ?transactions:((transactions:transaction list) = [])
  ?blocks:((blocks:block list) = [])
  ?peers:((peers:peer list) = [])
  () : post  = {
  transactions;
  blocks;
  peers;
}
