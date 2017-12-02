module BlockDB = Database.Make(Block)
module Cache = Map.Make(struct type t = int let compare = compare end)

type t = {hash : string;
          head : Block.t;
          cache : Block.t Cache.t;
          db : BlockDB.t}

let extend {hash; head; _} block =
  let 

let rec create ?cache_size:(cs = 10) db base head =
  if base.head = head then base
  else
    let%lwt {{prev_hash; _}; _} = BlockDB.get db head' in
    let parent = create db base prev_hash in
    

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
