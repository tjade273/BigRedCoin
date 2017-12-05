open Lwt.Infix
open Message_types
open P2p.BRCMessage_channel

module BlockDB = Chain.BlockDB

type t = {blockdb: BlockDB.t;
          head : Chain.t;
          forks: Chain.t list;
          p2p : P2p.t}

let initialize blockdb dir p2p =
  let gen_f = (Filename.concat dir "genesis.blk") in
  let chain_f = (Filename.concat dir "chains.dat") in
  let%lwt genesis = Lwt_io.(with_file ~mode:input gen_f)
      (fun ic -> Lwt_io.read ic >|= Block.deserialize)
  in
  let gen_chain = Chain.create blockdb genesis in
  Lwt_io.(with_file ~mode:output chain_f
            (fun oc -> write oc (Chain.serialize gen_chain)))
  >> Lwt.return {blockdb; head=gen_chain; forks = []; p2p}

let create dir p2p =
  let absolute = Filename.concat dir in
  let blockdb = BlockDB.create (absolute "blocks") in
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
    let forks = List.map Chain.deserialize (extract_chain 0) in
    let head = List.fold_left
        (fun c1 c2 -> if Chain.height c1 > Chain.height c2 then c1 else c2)
        (List.hd forks) forks
    in
    Lwt.return {head; forks; blockdb; p2p}
  else
    initialize blockdb dir p2p

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

let serve_blocks {blockdb; head; forks; _} oc startblocks height =
  let empty_message =  {method_= Post;
                        post = Some {transactions=[]; blocks=[]};
                        get = None; manage = None}
  in
  if height >= Chain.height head
  then
   write oc empty_message >|= ignore
  else
    let blocks = List.mapi (fun i x -> (height - 16*i, x)) startblocks in
    let same_hash (index, hash) = Chain.block_at_index head index
      >|= fun b -> Block.hash b = hash
    in
    let%lwt (index, shared_root) = Lwt_list.find_s same_hash blocks in
    let blocks_to_send = List.map Block.messageify (Chain.revert head shared_root |> fst) in
    post_blocks blocks_to_send oc

let handle_message bc (ic,oc) {method_; get; post; _} =
  let handle_get {request; startblocks; block_height} =
    match request with
    | Peer -> Lwt.return_unit
    | Mempool -> failwith "Unimplemented"
    | Blocks -> serve_blocks bc oc startblocks block_height
  in
  let handle_post {blocks; _} = insert_blocks bc blocks in
  match method_, get, post with
  | Get, Some msg, _ -> handle_get msg
  | Post, _, Some msg -> handle_post msg
  | _ -> Lwt.return_unit

let sync_with_peer bc (ic, oc) =
  let start_blocks = List.map (fun c -> Chain.hash c) bc.forks in
  let block_req = {request = Blocks;
                   startblocks = start_blocks}
  in
  let message = {method_ = Get;
                 get = Some block_req;
                 post = None;
                 manage = None}
  in
  write oc message >|= ignore >>
  match%lwt read ic with
  | None -> Lwt.return_unit
  | Some msg -> handle_message bc (ic,oc) msg
