[@@@ocaml.warning "-27-30-39"]

type get_mutable = {
  mutable request : Message_types.get_request;
  mutable startblock : bytes option;
}

let default_get_mutable () : get_mutable = {
  request = Message_types.default_get_request ();
  startblock = None;
}

type transaction_output_mutable = {
  mutable amount : int;
  mutable address : bytes;
}

let default_transaction_output_mutable () : transaction_output_mutable = {
  amount = 0;
  address = Bytes.create 0;
}

type transaction_input_mutable = {
  mutable txid : bytes;
  mutable out_index : int;
  mutable signature : bytes;
}

let default_transaction_input_mutable () : transaction_input_mutable = {
  txid = Bytes.create 0;
  out_index = 0;
  signature = Bytes.create 0;
}

type transaction_mutable = {
  mutable outs : Message_types.transaction_output list;
  mutable ins : Message_types.transaction_input list;
}

let default_transaction_mutable () : transaction_mutable = {
  outs = [];
  ins = [];
}

type block_header_mutable = {
  mutable version : int;
  mutable prev_hash : bytes;
  mutable merkle_root : bytes;
  mutable nonce : int;
  mutable n_bits : int;
  mutable timestamp : int;
}

let default_block_header_mutable () : block_header_mutable = {
  version = 0;
  prev_hash = Bytes.create 0;
  merkle_root = Bytes.create 0;
  nonce = 0;
  n_bits = 0;
  timestamp = 0;
}

type block_mutable = {
  mutable header : Message_types.block_header;
  mutable txs : Message_types.transaction list;
  mutable tx_count : int;
}

let default_block_mutable () : block_mutable = {
  header = Message_types.default_block_header ();
  txs = [];
  tx_count = 0;
}

type peer_mutable = {
  mutable address : string;
  mutable port : int;
  mutable last_seen : int;
}

let default_peer_mutable () : peer_mutable = {
  address = "";
  port = 0;
  last_seen = 0;
}

type manage_mutable = {
  mutable manage_type : Message_types.manage_manage_t;
  mutable peers : Message_types.peer list;
}

let default_manage_mutable () : manage_mutable = {
  manage_type = Message_types.default_manage_manage_t ();
  peers = [];
}

type post_mutable = {
  mutable transactions : Message_types.transaction list;
  mutable blocks : Message_types.block list;
}

let default_post_mutable () : post_mutable = {
  transactions = [];
  blocks = [];
}

type message_mutable = {
  mutable method_ : Message_types.message_method;
  mutable get : Message_types.get option;
  mutable post : Message_types.post option;
  mutable manage : Message_types.manage option;
}

let default_message_mutable () : message_mutable = {
  method_ = Message_types.default_message_method ();
  get = None;
  post = None;
  manage = None;
}


let rec decode_get_request d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Message_types.Peer:Message_types.get_request)
  | 1 -> (Message_types.Mempool:Message_types.get_request)
  | 2 -> (Message_types.Blocks:Message_types.get_request)
  | _ -> Pbrt.Decoder.malformed_variant "get_request"

let rec decode_get d =
  let v = default_get_mutable () in
  let continue__= ref true in
  let request_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.request <- decode_get_request d; request_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(get), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.startblock <- Some (Pbrt.Decoder.bytes d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(get), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !request_is_set then Pbrt.Decoder.missing_field "request" end;
  ({
    Message_types.request = v.request;
    Message_types.startblock = v.startblock;
  } : Message_types.get)

let rec decode_transaction_output d =
  let v = default_transaction_output_mutable () in
  let continue__= ref true in
  let address_is_set = ref false in
  let amount_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.amount <- Pbrt.Decoder.int_as_varint d; amount_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction_output), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.address <- Pbrt.Decoder.bytes d; address_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction_output), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !address_is_set then Pbrt.Decoder.missing_field "address" end;
  begin if not !amount_is_set then Pbrt.Decoder.missing_field "amount" end;
  ({
    Message_types.amount = v.amount;
    Message_types.address = v.address;
  } : Message_types.transaction_output)

let rec decode_transaction_input d =
  let v = default_transaction_input_mutable () in
  let continue__= ref true in
  let signature_is_set = ref false in
  let out_index_is_set = ref false in
  let txid_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.txid <- Pbrt.Decoder.bytes d; txid_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction_input), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.out_index <- Pbrt.Decoder.int_as_varint d; out_index_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction_input), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.signature <- Pbrt.Decoder.bytes d; signature_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction_input), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !signature_is_set then Pbrt.Decoder.missing_field "signature" end;
  begin if not !out_index_is_set then Pbrt.Decoder.missing_field "out_index" end;
  begin if not !txid_is_set then Pbrt.Decoder.missing_field "txid" end;
  ({
    Message_types.txid = v.txid;
    Message_types.out_index = v.out_index;
    Message_types.signature = v.signature;
  } : Message_types.transaction_input)

let rec decode_transaction d =
  let v = default_transaction_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.ins <- List.rev v.ins;
      v.outs <- List.rev v.outs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.outs <- (decode_transaction_output (Pbrt.Decoder.nested d)) :: v.outs;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.ins <- (decode_transaction_input (Pbrt.Decoder.nested d)) :: v.ins;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(transaction), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Message_types.outs = v.outs;
    Message_types.ins = v.ins;
  } : Message_types.transaction)

let rec decode_block_header d =
  let v = default_block_header_mutable () in
  let continue__= ref true in
  let timestamp_is_set = ref false in
  let n_bits_is_set = ref false in
  let nonce_is_set = ref false in
  let merkle_root_is_set = ref false in
  let prev_hash_is_set = ref false in
  let version_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.version <- Pbrt.Decoder.int_as_varint d; version_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block_header), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.prev_hash <- Pbrt.Decoder.bytes d; prev_hash_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block_header), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.merkle_root <- Pbrt.Decoder.bytes d; merkle_root_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block_header), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.nonce <- Pbrt.Decoder.int_as_varint d; nonce_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block_header), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.n_bits <- Pbrt.Decoder.int_as_varint d; n_bits_is_set := true;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block_header), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.timestamp <- Pbrt.Decoder.int_as_varint d; timestamp_is_set := true;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block_header), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !timestamp_is_set then Pbrt.Decoder.missing_field "timestamp" end;
  begin if not !n_bits_is_set then Pbrt.Decoder.missing_field "n_bits" end;
  begin if not !nonce_is_set then Pbrt.Decoder.missing_field "nonce" end;
  begin if not !merkle_root_is_set then Pbrt.Decoder.missing_field "merkle_root" end;
  begin if not !prev_hash_is_set then Pbrt.Decoder.missing_field "prev_hash" end;
  begin if not !version_is_set then Pbrt.Decoder.missing_field "version" end;
  ({
    Message_types.version = v.version;
    Message_types.prev_hash = v.prev_hash;
    Message_types.merkle_root = v.merkle_root;
    Message_types.nonce = v.nonce;
    Message_types.n_bits = v.n_bits;
    Message_types.timestamp = v.timestamp;
  } : Message_types.block_header)

let rec decode_block d =
  let v = default_block_mutable () in
  let continue__= ref true in
  let tx_count_is_set = ref false in
  let header_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.txs <- List.rev v.txs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.header <- decode_block_header (Pbrt.Decoder.nested d); header_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.txs <- (decode_transaction (Pbrt.Decoder.nested d)) :: v.txs;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.tx_count <- Pbrt.Decoder.int_as_varint d; tx_count_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(block), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !tx_count_is_set then Pbrt.Decoder.missing_field "tx_count" end;
  begin if not !header_is_set then Pbrt.Decoder.missing_field "header" end;
  ({
    Message_types.header = v.header;
    Message_types.txs = v.txs;
    Message_types.tx_count = v.tx_count;
  } : Message_types.block)

let rec decode_peer d =
  let v = default_peer_mutable () in
  let continue__= ref true in
  let last_seen_is_set = ref false in
  let port_is_set = ref false in
  let address_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.address <- Pbrt.Decoder.string d; address_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(peer), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.port <- Pbrt.Decoder.int_as_varint d; port_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(peer), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.last_seen <- Pbrt.Decoder.int_as_varint d; last_seen_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(peer), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !last_seen_is_set then Pbrt.Decoder.missing_field "last_seen" end;
  begin if not !port_is_set then Pbrt.Decoder.missing_field "port" end;
  begin if not !address_is_set then Pbrt.Decoder.missing_field "address" end;
  ({
    Message_types.address = v.address;
    Message_types.port = v.port;
    Message_types.last_seen = v.last_seen;
  } : Message_types.peer)

let rec decode_manage_manage_t d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Message_types.Ping:Message_types.manage_manage_t)
  | 1 -> (Message_types.Pong:Message_types.manage_manage_t)
  | 2 -> (Message_types.Peer_d:Message_types.manage_manage_t)
  | 3 -> (Message_types.Peer_p:Message_types.manage_manage_t)
  | 4 -> (Message_types.Data_p:Message_types.manage_manage_t)
  | _ -> Pbrt.Decoder.malformed_variant "manage_manage_t"

let rec decode_manage d =
  let v = default_manage_mutable () in
  let continue__= ref true in
  let manage_type_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.peers <- List.rev v.peers;
    ); continue__ := false
    | Some (0, Pbrt.Varint) -> begin
      v.manage_type <- decode_manage_manage_t d; manage_type_is_set := true;
    end
    | Some (0, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(manage), field(0)" pk
    | Some (1, Pbrt.Bytes) -> begin
      v.peers <- (decode_peer (Pbrt.Decoder.nested d)) :: v.peers;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(manage), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !manage_type_is_set then Pbrt.Decoder.missing_field "manage_type" end;
  ({
    Message_types.manage_type = v.manage_type;
    Message_types.peers = v.peers;
  } : Message_types.manage)

let rec decode_post d =
  let v = default_post_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.blocks <- List.rev v.blocks;
      v.transactions <- List.rev v.transactions;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.transactions <- (decode_transaction (Pbrt.Decoder.nested d)) :: v.transactions;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(post), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.blocks <- (decode_block (Pbrt.Decoder.nested d)) :: v.blocks;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(post), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Message_types.transactions = v.transactions;
    Message_types.blocks = v.blocks;
  } : Message_types.post)

let rec decode_message_method d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Message_types.Get:Message_types.message_method)
  | 1 -> (Message_types.Post:Message_types.message_method)
  | 2 -> (Message_types.Manage:Message_types.message_method)
  | _ -> Pbrt.Decoder.malformed_variant "message_method"

let rec decode_message d =
  let v = default_message_mutable () in
  let continue__= ref true in
  let method__is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.method_ <- decode_message_method d; method__is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(message), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.get <- Some (decode_get (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(message), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.post <- Some (decode_post (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(message), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.manage <- Some (decode_manage (Pbrt.Decoder.nested d));
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(message), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !method__is_set then Pbrt.Decoder.missing_field "method_" end;
  ({
    Message_types.method_ = v.method_;
    Message_types.get = v.get;
    Message_types.post = v.post;
    Message_types.manage = v.manage;
  } : Message_types.message)

let rec encode_get_request (v:Message_types.get_request) encoder =
  match v with
  | Message_types.Peer -> Pbrt.Encoder.int_as_varint (0) encoder
  | Message_types.Mempool -> Pbrt.Encoder.int_as_varint 1 encoder
  | Message_types.Blocks -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_get (v:Message_types.get) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_get_request v.Message_types.request encoder;
  begin match v.Message_types.startblock with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  | None -> ();
  end;
  ()

let rec encode_transaction_output (v:Message_types.transaction_output) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.amount encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Message_types.address encoder;
  ()

let rec encode_transaction_input (v:Message_types.transaction_input) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Message_types.txid encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.out_index encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Message_types.signature encoder;
  ()

let rec encode_transaction (v:Message_types.transaction) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_transaction_output x) encoder;
  ) v.Message_types.outs;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_transaction_input x) encoder;
  ) v.Message_types.ins;
  ()

let rec encode_block_header (v:Message_types.block_header) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.version encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Message_types.prev_hash encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Message_types.merkle_root encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.nonce encoder;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.n_bits encoder;
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.timestamp encoder;
  ()

let rec encode_block (v:Message_types.block) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_block_header v.Message_types.header) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_transaction x) encoder;
  ) v.Message_types.txs;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.tx_count encoder;
  ()

let rec encode_peer (v:Message_types.peer) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Message_types.address encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.port encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.Message_types.last_seen encoder;
  ()

let rec encode_manage_manage_t (v:Message_types.manage_manage_t) encoder =
  match v with
  | Message_types.Ping -> Pbrt.Encoder.int_as_varint (0) encoder
  | Message_types.Pong -> Pbrt.Encoder.int_as_varint 1 encoder
  | Message_types.Peer_d -> Pbrt.Encoder.int_as_varint 2 encoder
  | Message_types.Peer_p -> Pbrt.Encoder.int_as_varint 3 encoder
  | Message_types.Data_p -> Pbrt.Encoder.int_as_varint 4 encoder

let rec encode_manage (v:Message_types.manage) encoder = 
  Pbrt.Encoder.key (0, Pbrt.Varint) encoder; 
  encode_manage_manage_t v.Message_types.manage_type encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_peer x) encoder;
  ) v.Message_types.peers;
  ()

let rec encode_post (v:Message_types.post) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_transaction x) encoder;
  ) v.Message_types.transactions;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_block x) encoder;
  ) v.Message_types.blocks;
  ()

let rec encode_message_method (v:Message_types.message_method) encoder =
  match v with
  | Message_types.Get -> Pbrt.Encoder.int_as_varint (0) encoder
  | Message_types.Post -> Pbrt.Encoder.int_as_varint 1 encoder
  | Message_types.Manage -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_message (v:Message_types.message) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_message_method v.Message_types.method_ encoder;
  begin match v.Message_types.get with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_get x) encoder;
  | None -> ();
  end;
  begin match v.Message_types.post with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_post x) encoder;
  | None -> ();
  end;
  begin match v.Message_types.manage with
  | Some x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_manage x) encoder;
  | None -> ();
  end;
  ()
