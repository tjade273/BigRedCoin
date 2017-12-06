open OUnit2
open Chain
open Block
open Lwt

let db = BlockDB.create "chain_test.db"

let rec mine_header header target =
  let h = {header with
           timestamp = int_of_float (Unix.time ());
           nonce = header.nonce + 1}
  in
  let b = {header = h;
           transactions=[];
           transactions_count = 0}
  in
  if Block.hash b < target then b
  else mine_header h target

let initial_difficulty = 0x1f00ffff

let genesis = mine_header {
  version = 0;
  prev_hash = String.make 32 '\x00';
  merkle_root = String.make 32 '\x00';
  nonce = 0;
  nBits = initial_difficulty;
  timestamp = int_of_float (Unix.time ());
} (Block.target initial_difficulty)

let blockchain =
  print_endline "Generating blocks....";
  let rec make_chain n acc =
    if n = 0 then acc
    else
      let prv = List.hd acc in
      let target = Block.target prv.header.nBits in
      let block = mine_header {prv.header with prev_hash = Block.hash prv} target in
      make_chain (n-1) (block::acc)
  in
  make_chain 5 [genesis]

let id s = Hex.of_string s |>  function `Hex x -> x

let%lwt () = Lwt_list.iter_s (BlockDB.put db) blockchain

let opt_exn = function
  | Some x -> x
  | None -> failwith "Unexpected none"


let%lwt c1 = Lwt_list.fold_left_s (fun c b -> (extend c b >|= opt_exn))
    (create db genesis)
    (blockchain |> List.rev |> List.tl)

let new_block = {genesis with header = {genesis.header with prev_hash = hash @@ Chain.head c1}}

let%lwt c2 = extend c1 genesis
let%lwt c3 = extend c1 new_block
let%lwt c4 = extend c1 (mine_header new_block.header (Block.target initial_difficulty)) >|= opt_exn
let%lwt c5 = extend c1 {(Chain.head c4) with header = {(Chain.head c4).header with nBits = initial_difficulty - 1}}

let%lwt b3 = block_at_index c1 3

let tests = "Chain Tests" >::: [
    "mine_genesis" >:: (fun _ -> assert_equal 0 (create db genesis |> height));
    "extend_chain" >:: (fun _ -> assert_equal (Block.hash  (List.hd blockchain)) (Chain.head c1 |> Block.hash));
    "extend_height" >:: (fun _ -> assert_equal 5 (height c1));
    "extend_out_of_order" >:: (fun _ -> assert_equal None c2);
    "extend_bad_target" >:: (fun _ -> assert_equal None c3);
    "extend_bad_nbits" >:: (fun _ -> assert_equal None c5);
    "block_at_index" >:: (fun _ -> assert_equal (List.nth blockchain 2) b3);
    "revert_to_self" >:: (fun _ -> assert_equal [] (fst (Chain.revert c1 (Chain.hash c1))));
    "revert_two" >:: (fun _ -> assert_equal (Block.hash b3) (Chain.hash (snd (Chain.revert c1 (Block.hash b3)))))
  ]
