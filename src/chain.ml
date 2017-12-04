open Block
open Lwt

module BlockDB = Database.Make(Block)
module Cache = Map.Make(String)

let cache_size = 2048

type t = {hash : string;
          head : Block.t;
          genesis : Block.t;
          height : int;
          cache : (int * Block.t) Cache.t;
          db : BlockDB.t}

(* Gets the block at index [i]. [i] cant be negative or more than height *)
let get_n {hash; head; height; cache; db} (n : int) =
  let nth_opt =
    List.find_opt (fun (_, (i,_)) -> i = n) (Cache.bindings cache)
  in
  match nth_opt with
  | Some (_, (_, block)) -> Lwt.return block
  | None ->
    let rec parent block n =
      if n = 0 then Lwt.return block else
      let%lwt p = BlockDB.get db Block.(block.header.prev_hash) in
      parent p (n - 1)
    in
    parent head (height - n)

(* doesn't validate txs yet *)
let extend ({hash; head; height; cache; _} as chain) new_block =
  let%lwt reference = get_n chain (height - 2016) in
  let nbits' = Block.(next_difficulty head.header reference.header) in
  let target = Block.target nbits' in
  let blockhash = Block.hash new_block  in
  if blockhash > target then Lwt.return_none
  else
    let cache' = Cache.add blockhash (height+1, new_block) cache in
    let cache' = Cache.filter (fun _ (i, _) -> height - i <= 2048) cache' in
    Lwt.return_some
      { chain with
        hash = blockhash;
        head = new_block;
        height = height + 1;
        cache = cache';
      }

let rec create base head =
  if base.hash = head then Lwt.return_some base
  else
    let%lwt {header; _} as block = BlockDB.get base.db head in
    match%lwt create base header.prev_hash with
    | None -> Lwt.return_none
    | Some parent -> extend parent block

let extend_cache {cache; db; _} =
  let no_parent h =
    let (_, {header;_}) = Cache.find h cache in
    not (Cache.mem header.prev_hash cache)
  in
  let (_, oldest) = Cache.find_first no_parent cache in
  let rec add_parent cache (height, child) n =
    if n = 0 then Lwt.return cache
    else
      let%lwt parent = BlockDB.get db child.header.prev_hash in
      let cache' = Cache.add child.header.prev_hash (height - 1, parent) cache in
      add_parent cache' (height - 1, parent) (n-1)
  in
  add_parent cache oldest 25

let rec shared_root c1 c2 =
  let highest_block h (i,b) acc =
    match Cache.find_opt h c2.cache, acc with
    | None, _ -> acc
    | Some x, None -> Some x
    | Some (height, block), Some (height', block') ->
      begin
        if height' > height
        then Some (height', block')
        else Some (height, block)
      end
  in
  match Cache.fold highest_block c1.cache None with
  | Some (i, b) -> Lwt.return b
  | None ->
    let%lwt c1' = extend_cache c1 in
    let%lwt c2' = extend_cache c2 in
    shared_root c1 c2

let rec revert ({hash; head; height; cache; db} as c) h =
  if hash = h
  then ([], {c with cache = Cache.empty})
  else
    let prev_hash = head.header.prev_hash in
    let _, parent = Cache.find prev_hash cache in
    let (blocks, chain) = revert {c with head = parent;
                                         hash = prev_hash;
                                         height = (height-1)
                                 } h
    in
    head::blocks, chain
