open Lwt_test
open Block
open Chain
open Lwt.Infix

let dir = "test_blockchain/"

let show = function `Hex x -> x

let () = Unix.mkdir dir 0o777

let () =
  try
    Sys.remove (dir^"chains.dat"); Sys.remove (dir^"genesis.blk")
  with Sys_error _ -> ()

let%lwt () = Lwt_io.with_file ~mode:Lwt_io.output (dir^"genesis.blk") (fun oc -> Lwt_io.write oc (Block.serialize Chain_test.genesis))

let%lwt peer1 = P2p.create_from_list ~port:3000 ["127.0.0.1", 4001, None]
let%lwt peer2 = P2p.create_from_list ~port:3001 ["127.0.0.1", 4000, None]
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
    end;

    test "get utxos" begin fun () ->
      let utxos = Blockchain.get_utxos !blockchain (String.make 20 '\x00') in
      Lwt.return Transaction.((snd @@ List.hd utxos).amount = 25)
    end
  ]]
