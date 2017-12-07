open Lwt.Infix
open Message_types
open P2p.BRCMessage_channel

module BlockDB = Chain.BlockDB
module TxDB = Utxo_pool.TransactionDB

exception Invalid_block

type t = {blockdb: BlockDB.t;
          head : Chain.t;
          forks: Chain.t list;
          p2p : P2p.t;
          utxos : Utxo_pool.t;
          mempool : Transaction.t list;
          dir : string}

(* [initialize blockdb dir p2p] is a fresh blockchain initialized to the gensesis,
 * using [blockdb] as a backend and located in [dir] *)
let initialize blockdb txdb dir p2p =
  let gen_f = (Filename.concat dir "genesis.blk") in
  let chain_f = (Filename.concat dir "chains.dat") in
  let%lwt genesis = Lwt_io.(with_file ~mode:input gen_f)
      (fun ic -> Lwt_io.read ic >|= Block.deserialize)
  in
  BlockDB.put blockdb genesis >>
  let gen_chain = Chain.create blockdb genesis in
  Lwt_io.(with_file ~mode:output chain_f
            (fun oc -> write oc (Chain.serialize gen_chain)))
  >> Lwt.return {blockdb;
                 head=gen_chain;
                 forks = [];
                 mempool = [];
                 utxos = Utxo_pool.empty txdb;
                 p2p; dir}

(* [create dir p2p] is a blockchain with resources stored in [dir], using
 * [p2p] as a p2p instance. *)
let create dir p2p =
  (if%lwt Lwt_unix.file_exists dir >|= not
   then Lwt_unix.mkdir dir 0o700
   else Lwt.return_unit) >>
  let absolute = Filename.concat dir in
  let blockdb = BlockDB.create (absolute "blocks") in
  let txdb = TxDB.create (absolute "transactions") in
  let chain_f = absolute "chains.dat" in
  if%lwt Lwt_unix.file_exists chain_f
  then
    let%lwt ic = Lwt_io.(open_file ~mode:input chain_f) in
    let%lwt chain_s = Lwt_io.read ic in
    Lwt_io.close ic >>
    let rec extract_chain i =
      if i = String.length chain_s
      then []
      else (String.sub chain_s i 40)::(extract_chain (i+40))
    in
    let%lwt forks = Lwt_list.map_p (Chain.deserialize blockdb) (extract_chain 0) in
    let head = List.fold_left
        (fun c1 c2 -> if Chain.height c1 > Chain.height c2 then c1 else c2)
        (List.hd forks) forks
    in
    Lwt.return {head; forks;
                utxos = Utxo_pool.empty txdb;
                mempool = [];
                blockdb; p2p; dir}
  else
    initialize blockdb txdb dir p2p

let close_blockchain blockchain =
  let bc = !blockchain in
  let absolute = Filename.concat bc.dir in
  let chain_f = absolute "chains.dat" in
  Lwt_io.(with_file ~mode:output chain_f
            (fun oc -> write oc (Chain.serialize bc.head)))


(* [post_blocks blocks oc] pushes the list of blocks [blocks] across the
 * p2p channel [oc], 128 at a time. *)
let post_blocks blocks oc =
  let post_msg block_list =
    {method_ = Post;
     post = Some {transactions = [];
                  blocks = block_list};
     get = None; manage = None
    }
  in
  let rec pop n l =
    match l with
    | [] -> ([], [])
    | x::xs when n = 0 -> ([], l)
    | x::xs -> let (h, t) = pop (n-1) xs in (x::h, t)
  in
  let rec send_n n l =
    match l with
    | [] -> Lwt.return_unit
    | _ ->
      begin
        let (h, t) = pop n l in
        write oc (post_msg h) >>= fun _ ->
        send_n n t
      end
  in
  send_n 128 blocks

(* [serve_blocks blockchain oc startblocks height] services a
 * request by the peer at the end of the channel [oc]. [startblocks] is a list of
 * block hashes starting at [height] and progressing down the peer's chain in increments
 * of 16 *)
let serve_blocks {blockdb; head; forks; _} oc startblocks height =
  let empty_message =  {method_= Post;
                        post = Some {transactions=[]; blocks=[]};
                        get = None; manage = None}
  in
  if height >= Chain.height head
  then
    Lwt_log.notice "too high" >>
   write oc empty_message >|= ignore
  else
    Lwt_log.notice "Good height" >>
    let blocks = List.mapi (fun i x -> (height - 16*i, x)) startblocks in
    let same_hash (index, hash) = Chain.block_at_index head index
      >|= fun b -> Block.hash b = hash
    in
    let%lwt (index, shared_root) = Lwt_list.find_s same_hash blocks in
    let blocks_to_send = List.map Block.messageify (Chain.revert head shared_root |> fst) in
    Lwt_log.notice ("Sending "^(string_of_int (List.length blocks_to_send))) >>
    post_blocks blocks_to_send oc

let reorganize_chain bc new_chain root =
  try
    let root_hash = Block.hash root in
    let undo, _ = Chain.revert bc.head root_hash in
    let todo, _ = Chain.revert new_chain root_hash in
    let%lwt utxos = Lwt_list.fold_left_s Utxo_pool.revert bc.utxos undo in
    let%lwt utxos = Lwt_list.fold_left_s Utxo_pool.apply utxos todo in
    Lwt.return {bc with head = new_chain; utxos}
  with Utxo_pool.Invalid_block | Not_found ->
    Lwt.return bc

(* [insert_blocks bc blocks] is [bc] after attempting to sequentially
 * insert [blocks] into the chain. The first element of [blocks] should be
 * an element of [bc.head] *)
let insert_blocks bc blocks =
  let open Block in
  Lwt_list.iter_p (BlockDB.put bc.blockdb) blocks >>
  match blocks with
  | [] -> Lwt.return bc
  | root::heads ->
    begin
      let {header; transactions; transactions_count} = root in
      if List.length transactions <> transactions_count
      then Lwt.fail Invalid_block
      else
        let _, chain = Chain.revert bc.head (Block.hash root) in
        let try_extend chain block =
          match%lwt Chain.extend chain block with
          | None -> Lwt.fail Invalid_block
          | Some c -> Lwt.return c
        in
        let%lwt new_chain =
          Lwt.catch (fun () -> Lwt_list.fold_left_s try_extend chain heads)
            (fun exn -> Lwt.return bc.head)
        in
        if Chain.height new_chain > Chain.height bc.head
        then reorganize_chain bc new_chain root
        else Lwt.return {bc with forks = new_chain::bc.forks}
    end

(* [handle_message bc (ic, oc) msg] is [bc] after responding to the message [msg]
 * If [msg] is not a post request, [bc] is unchanged *)
let handle_message bc (ic,oc) {method_; get; post; _} : t Lwt.t =
  let handle_get {request; startblocks; block_height} =
    Lwt_log.notice "Got get">>
    Lwt_log.notice "Test" >>
    match request with
    | Peer -> Lwt.return_unit
    | Mempool -> failwith "Unimplemented"
    | Blocks -> Lwt_log.notice "Blocks request" >> serve_blocks bc oc startblocks block_height
  in
  let handle_post {blocks; _} =
    Lwt_log.notice "Got post">>
    Lwt.catch (fun () ->
        insert_blocks bc (List.map Block.demessageify blocks))
      (fun exn -> Lwt_log.debug "Bad block received\n" >> Lwt.return bc)
  in
  match method_, get, post with
  | Get, Some msg, _ -> handle_get msg >> Lwt.return bc
  | Post, _, Some msg -> handle_post msg
  | _ -> Lwt.return bc

(* [read_messages (ic, oc) blockchain] is [blockchain] after reading all available
 * messages from the channel pair*)
let rec read_messages (ic, oc) bc : t Lwt.t =
  Lwt_unix.sleep 0. >>
  match%lwt read ~timeout:5. ic with
  | None -> Lwt.return bc
  | Some msg -> Lwt_log.notice "Got bc message" >> handle_message bc (ic, oc) msg
    >>= read_messages (ic, oc)

(* After [sync_wit_peer blockchain channel], either [blockchain] is updated
 * to be closer to the blockchain of the peer at the other side of [channel]
 * or vice versa *)
let sync_with_peer ({head; _} as bc) (ic, oc) =
  Lwt_log.notice "Peer found.." >>
  let rec checkpoints n  =
    print_endline (string_of_int n);
    match n with
    | 0 -> Lwt.return [Chain.hash head]
    | _ ->
      let%lwt hash = Chain.block_at_index head (Chain.height head - n) >|= Block.hash in
      let%lwt tl = checkpoints (n - 16) in
      Lwt.return (hash::tl)
  in
  let num_checks = min 10 ((Chain.height head)/16) in
  let%lwt block_hashes = checkpoints (num_checks) in
  Lwt_log.notice "Got checkpoints" >>
  let block_req = {request = Blocks;
                   startblocks = block_hashes;
                   block_height = Chain.height head}
  in
  let message = {method_ = Get;
                 get = Some block_req;
                 post = None;
                 manage = None}
  in
  Lwt_log.notice "Writing message" >>
  (write oc message >>= (fun i -> Lwt_log.notice (string_of_int i))) >>
  Lwt_log.notice "Wrote message" >>
  read_messages (ic, oc) bc

let rec sync blockchain =
  Lwt_log.notice "Started syncing" >>
  let bc = !blockchain in
  let peer_stream = P2p.peer_stream bc.p2p in
  match%lwt Lwt_stream.get peer_stream with
  | None -> Lwt_log.notice "closed" >> close_blockchain blockchain
  | Some peer ->
    (bc.p2p,peer) @<> (fun peer -> 
    Lwt_log.notice "Syncing with peer..." >>
    let ic = P2p.BRCPeer.ic peer in
    let oc = P2p.BRCPeer.oc peer in
    let%lwt bc' = sync_with_peer bc (ic,oc) in
    blockchain := bc'; Lwt_main.yield () >>
    sync blockchain >> Lwt.return(true,Lwt.return_unit))

let push_block blockchain block =
  let open Block in
  let bc = !blockchain in
  BlockDB.put bc.blockdb block >>
  let%lwt parent = BlockDB.get bc.blockdb block.header.prev_hash in
  let%lwt bc' = Lwt.catch (fun () -> insert_blocks bc [parent; block])
      (fun exn -> Lwt_log.notice "Mined invalid block" >> Lwt.return bc) in
  blockchain := bc';
  Lwt.return_unit

let head {head; } =
  Chain.hash head

(* Todo: actually include txs *)
let next_block {head; _} =
  let open Block in
  let prev_hash = Chain.hash head in
  let%lwt nBits = Chain.next_difficulty head in
  let header = {
    version = 0;
    prev_hash;
    merkle_root = String.make 32 '\x00';
    nonce = 0;
    nBits;
    timestamp = 0}
  in
  Lwt.return {header; transactions = []; transactions_count = 0}

let get_utxos {utxos; _} address =
  Utxo_pool.filter utxos address

let retrieve_block bc hash = 
  BlockDB.get_opt bc.blockdb hash
  
let chain {head; _} = head
