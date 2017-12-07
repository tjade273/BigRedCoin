open Transaction
open Block
open Lwt_test
open Miner
open Lwt

let suites : Lwt_test.suite list = []

let header1 = {
  version = 1;
  prev_hash = Crypto.sha256 "hi";
  merkle_root = "1";
  nonce = 3;
  nBits = 0x181bc331;
  timestamp = 1512183567
}

let header2 = {
  version = 1;
  prev_hash = Crypto.sha256 "hi!";
  merkle_root = "2";
  nonce = 3;
  nBits = 0x181bc330;
  timestamp = 1512180567
}

let header3 = {
  header1 with
  timestamp = header1.timestamp + 30*2016
}

let header4 = {
  header1 with
  timestamp = header1.timestamp + 30*2016*5
}

let header5 = {
  header1 with
  timestamp = header1.timestamp + 2*2016
}

let header6 = {
  header1 with
  timestamp = header1.timestamp + (30*2016*3)/2
}

let header7 = {
  header1 with
  timestamp = header1.timestamp;
  nBits = 0x18fbc330
}

let input1 = {
  txid = Crypto.sha256 "hola";
  out_index = 5
}

let output1 = {
  amount = 3;
  address = Crypto.sha256 "hello"
}

let transaction1 = {
  outs = [output1];
  ins = [input1];
  sigs = None
}

let block1 = {
  header = header1;
  transactions = [transaction1];
  transactions_count = 1
}

let dir = "test_miner/"

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

let%lwt peer1 = P2p.create_from_list ~port:3333 ["127.0.0.1", 4001, None]
let () =
  try
    Unix.mkdir "test_miner/" 0o777
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let%lwt bc = Blockchain.create "test_miner" peer1

let blockchain = ref bc

let (stream, push) = Lwt_stream.create ()
let miner = create "lucas" push blockchain

let miner_tests = suite "miner tests" [ 
    test "equiv" begin fun () ->
      Lwt.return (Miner.equiv Block_test.block1 {Block_test.block1 with
        transactions = [{
          outs = [output1];
          ins = [input1];
          sigs = Some ["a"]
        }]
     })
    end;

    test "mine_block" begin fun () ->
      print_endline "mining test";
      start miner >>
      Lwt_log.info "started" >>
      Lwt.pick [
        (Lwt_unix.sleep 120.0 >> begin stop miner; print_endline "Warning: block not mined within 120 seconds. Not necessarily a failure."; Lwt.return true end);
        (Lwt_stream.get stream >>= fun _ -> Lwt_stream.get stream >>= fun x -> (stop miner; Lwt.return true))
      ] >>= fun x -> begin Lwt.return x end
    end;
  ]

let _ = stop miner

let tests = suites @ [miner_tests]
