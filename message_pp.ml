[@@@ocaml.warning "-27-30-39"]

let rec pp_get_request fmt (v:Message_types.get_request) =
  match v with
  | Message_types.Peer -> Format.fprintf fmt "Peer"
  | Message_types.Mempool -> Format.fprintf fmt "Mempool"
  | Message_types.Blocks -> Format.fprintf fmt "Blocks"

let rec pp_get fmt (v:Message_types.get) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "request" pp_get_request fmt v.Message_types.request;
    Pbrt.Pp.pp_record_field "startblock" (Pbrt.Pp.pp_option Pbrt.Pp.pp_bytes) fmt v.Message_types.startblock;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_transaction_output fmt (v:Message_types.transaction_output) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "amount" Pbrt.Pp.pp_int64 fmt v.Message_types.amount;
    Pbrt.Pp.pp_record_field "address" Pbrt.Pp.pp_bytes fmt v.Message_types.address;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_transaction_input fmt (v:Message_types.transaction_input) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "txid" Pbrt.Pp.pp_bytes fmt v.Message_types.txid;
    Pbrt.Pp.pp_record_field "out_index" Pbrt.Pp.pp_int32 fmt v.Message_types.out_index;
    Pbrt.Pp.pp_record_field "signature" Pbrt.Pp.pp_bytes fmt v.Message_types.signature;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_transaction fmt (v:Message_types.transaction) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "outs" (Pbrt.Pp.pp_list pp_transaction_output) fmt v.Message_types.outs;
    Pbrt.Pp.pp_record_field "ins" (Pbrt.Pp.pp_list pp_transaction_input) fmt v.Message_types.ins;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_block_header fmt (v:Message_types.block_header) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "version" Pbrt.Pp.pp_int32 fmt v.Message_types.version;
    Pbrt.Pp.pp_record_field "prev_hash" Pbrt.Pp.pp_bytes fmt v.Message_types.prev_hash;
    Pbrt.Pp.pp_record_field "merkle_root" Pbrt.Pp.pp_bytes fmt v.Message_types.merkle_root;
    Pbrt.Pp.pp_record_field "nonce" Pbrt.Pp.pp_int64 fmt v.Message_types.nonce;
    Pbrt.Pp.pp_record_field "n_bits" Pbrt.Pp.pp_int64 fmt v.Message_types.n_bits;
    Pbrt.Pp.pp_record_field "timestamp" Pbrt.Pp.pp_int64 fmt v.Message_types.timestamp;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_block fmt (v:Message_types.block) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "header" pp_block_header fmt v.Message_types.header;
    Pbrt.Pp.pp_record_field "txs" (Pbrt.Pp.pp_list pp_transaction) fmt v.Message_types.txs;
    Pbrt.Pp.pp_record_field "tx_count" Pbrt.Pp.pp_int32 fmt v.Message_types.tx_count;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_peer fmt (v:Message_types.peer) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "address" Pbrt.Pp.pp_string fmt v.Message_types.address;
    Pbrt.Pp.pp_record_field "port" Pbrt.Pp.pp_int fmt v.Message_types.port;
    Pbrt.Pp.pp_record_field "last_seen" Pbrt.Pp.pp_int fmt v.Message_types.last_seen;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_manage_manage_t fmt (v:Message_types.manage_manage_t) =
  match v with
  | Message_types.Ping -> Format.fprintf fmt "Ping"
  | Message_types.Pong -> Format.fprintf fmt "Pong"
  | Message_types.Peer_d -> Format.fprintf fmt "Peer_d"
  | Message_types.Peer_p -> Format.fprintf fmt "Peer_p"
  | Message_types.Data_p -> Format.fprintf fmt "Data_p"

let rec pp_manage fmt (v:Message_types.manage) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "manage_type" pp_manage_manage_t fmt v.Message_types.manage_type;
    Pbrt.Pp.pp_record_field "peers" (Pbrt.Pp.pp_list pp_peer) fmt v.Message_types.peers;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_post fmt (v:Message_types.post) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "transactions" (Pbrt.Pp.pp_list pp_transaction) fmt v.Message_types.transactions;
    Pbrt.Pp.pp_record_field "blocks" (Pbrt.Pp.pp_list pp_block) fmt v.Message_types.blocks;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_message_method fmt (v:Message_types.message_method) =
  match v with
  | Message_types.Get -> Format.fprintf fmt "Get"
  | Message_types.Post -> Format.fprintf fmt "Post"
  | Message_types.Manage -> Format.fprintf fmt "Manage"

let rec pp_message fmt (v:Message_types.message) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "method_" pp_message_method fmt v.Message_types.method_;
    Pbrt.Pp.pp_record_field "get" (Pbrt.Pp.pp_option pp_get) fmt v.Message_types.get;
    Pbrt.Pp.pp_record_field "post" (Pbrt.Pp.pp_option pp_post) fmt v.Message_types.post;
    Pbrt.Pp.pp_record_field "manage" (Pbrt.Pp.pp_option pp_manage) fmt v.Message_types.manage;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
