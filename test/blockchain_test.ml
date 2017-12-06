open Lwt_test
open Block
open Chain
open Lwt.Infix

let dir = "test_blockchain/"

let show = function `Hex x -> x

let () =
  try
    Sys.remove (dir^"chains.dat"); Sys.remove (dir^"genesis.blk")
  with Sys_error _ -> ()

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


let%lwt () = Lwt_io.with_file ~mode:Lwt_io.output (dir^"genesis.blk") (fun oc -> Lwt_io.write oc (Block.serialize Chain_test.genesis))

let%lwt peer1 = P2p.create_from_list ~port:4000 ["127.0.0.1", 4001, None]
let%lwt peer2 = P2p.create_from_list ~port:4001 ["127.0.0.1", 4000, None]
let%lwt bc = Blockchain.create "test_blockchain" peer1

let%lwt () = Lwt_log.notice ("Genesis hash: "^(Hex.of_string (Blockchain.head bc) |> show))

let blockchain  = ref bc

let tests = [suite "blockchain tests" [
    test "test_sync" begin fun () ->
      let _ = Blockchain.sync blockchain in
      Lwt_log.notice "Started syncing" >>
      Lwt.return_true
    end;

    test "push block" begin fun () ->
      Lwt_log.notice ("Block 1: "^ (Hex.of_string (List.nth Chain_test.blockchain 4).header.prev_hash |> show)) >>
      let%lwt () = Blockchain.push_block blockchain (List.nth Chain_test.blockchain 4) in
      Lwt.return (Blockchain.head !blockchain = Block.hash @@ List.nth Chain_test.blockchain 4)
    end
  ]]
