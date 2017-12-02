open Block

module BlockDB = Database.Make(Block)
module Cache = Map.Make(struct type t = int let compare = compare end)

let cache_size = 2048

type t = {hash : string;
          head : Block.t;
          height : int;
          cache : Block.t Cache.t;
          db : BlockDB.t}

(* Gets the block at index [i]. [i] cant be negative or more than height *)
let get_n {hash; head; height; cache; db} n =
  match Cache.find_opt n cache with
  | Some block -> Lwt.return block
  | None ->
    let (i, b) = Cache.min_binding cache in
    let rec parent block n =
      if n = 0 then Lwt.return block else
      let%lwt p = BlockDB.get db Block.(block.header.prev_hash) in
      parent p (n-1)
    in
    parent b (i - n)

(* doesn't validate txs yet *)
let extend ({hash; head; height; cache; db} as chain) new_block =
  let%lwt reference = get_n chain (height - 2016) in
  let nbits' = Block.(next_difficulty head.header reference.header) in
  let target = Block.target nbits' in
  let blockhash = Block.hash new_block  in
  if blockhash > target then Lwt.return_none
  else
    let cache' = Cache.remove (height - cache_size) (Cache.add (height + 1) new_block cache) in
    Lwt.return_some
    {hash = blockhash;
     head = new_block;
     height = height + 1;
     cache = cache';
     db
     }

let rec create db base head =
  if base.hash = head then Lwt.return_some base
  else
    let%lwt {header; _} as block = BlockDB.get db head in
    match%lwt create db base header.prev_hash with
    | None -> Lwt.return_none
    | Some parent -> extend parent block

let shared_root c1 c2 =
  try
    List.find
      (fun (k,_) -> M.find_opt k c2.recent_blocks <> None)
      (M.bindings c1.recent_blocks) |> fst
  with Not_found ->
    shared_root (extend_cache c1) (extend_cache c2)

let reorg c1 c2 =
  let root = shared_root c1 c2 in
  let h1, h2 = M.find root c1.recent_blocks, M.find root c2.recent_blocks in
  let revert = List.filter ((hash, height))
