open Lwt_test
open Block
open Chain
open Lwt.Infix

(* TODO: MAKE MINER TEST DIR *)

let dir = "test_blockchain/"

let show = function `Hex x -> x

let () =
  try
    Unix.mkdir dir 0o777
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let () =
  try
    Sys.remove (dir^"chains.dat"); Sys.remove (dir^"genesis.blk")
  with Sys_error _ -> ()

let%lwt () = Lwt_io.with_file ~mode:Lwt_io.output (dir^"genesis.blk") (fun oc -> Lwt_io.write oc (Block.serialize Chain_test.genesis))

let () = print_endline (Hex.of_string Chain_test.genesis.header.merkle_root |> show)

let (pubkey, privkey) = Crypto.ECDSA.create ()
let address = Crypto.ECDSA.to_address pubkey

let%lwt peer1 = P2p.create_from_list ~port:3000 ["127.0.0.1", 3001, None]
let%lwt peer2 = P2p.create_from_list ~port:3001 ["127.0.0.1", 3000, None]
let%lwt bc = Blockchain.create "test_blockchain" peer1

let%lwt () = Lwt_log.notice ("Genesis hash: "^(Hex.of_string (Blockchain.head bc) |> show))


let block1 = List.nth (List.rev Chain_test.blockchain) 1

let coinbase_tx = Transaction.({ins = []; outs = [{amount = 25; address}]; sigs = Some [Crypto.random 32]})
let tx1_nosig = Transaction.({outs = [{amount = 12; address}; {amount = 13; address = String.make 32 '\x00'}]; ins = [{txid = Transaction.hash coinbase_tx; out_index = 0}]; sigs = None})
let sig1 = Crypto.ECDSA.(sign (pubkey, privkey) (Transaction.serialize tx1_nosig) |> string_of_sig)

let tx1 = {tx1_nosig with Transaction.sigs = Some [sig1]}

let header1 = {block1.header with prev_hash = Block.hash block1}

let rec mine_tx_block header target txs =
  let open Transaction in
  let h = {header with
           timestamp = int_of_float (Unix.time ());
           nonce = header.nonce + 1;
          }
  in
  let b = {header = h;
           transactions=txs;
           transactions_count = List.length txs}
  in
  let b = {b with header = {header with merkle_root = merkle_root b.transactions}} in
  if Block.hash b < target then b
  else mine_tx_block h target txs

let block2 = mine_tx_block header1 (Block.target header1.nBits) [coinbase_tx; tx1]

let blockchain  = ref bc

let tests = [suite "blockchain tests" [
    test "test_sync" begin fun () ->
      let _ = Blockchain.sync blockchain in
      Lwt_log.notice "Started syncing" >>
      Lwt.return_true
    end;

    test "push block" begin fun () ->
      let%lwt () = Blockchain.push_block blockchain block1 in
      Lwt.return (Blockchain.head !blockchain = Block.hash block1)
    end;

    test "get utxos" begin fun () ->
      let utxos = Blockchain.get_utxos !blockchain (String.make 20 '\x00') in
      Lwt.return Transaction.((snd @@ List.hd utxos).amount = 25)
    end;

    test "spend utxos" begin fun () ->
      let%lwt () = Blockchain.push_block blockchain block2 in
      let utxos = Blockchain.get_utxos !blockchain address in
      Lwt.return Transaction.(utxos = [({txid = Transaction.hash tx1; out_index = 0}, {amount = 12; address})])
    end
  ]]
