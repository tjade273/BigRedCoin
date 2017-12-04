open Lwt.Infix

module BlockDB = Chain.BlockDB

type t = {blockdb: BlockDB.t; head : Chain.t; forks: Chain.t list}

(* [initialize dir] is an lwt thread that returns a unit
 * only when [dir] contains all files that must exist for
 * operation of the node. *)
let initialize dir =
  let parse_genesis ic =
    Lwt_io.read ic >>= fun s -> Lwt.return (Block.deserialize s)
  in
  let absolute = Filename.concat dir in
  let blockdb = BlockDB.create (absolute "blocks") in
  let with_genesis = Lwt_io.(with_file ~mode:input (absolute "genesis.blk")) in
  let%lwt genesis = with_genesis parse_genesis in
  BlockDB.put blockdb genesis >>
  (if%lwt Lwt_unix.file_exists "head.dat" >|= not then
    let%lwt oc = Lwt_io.(open_file output "head.dat") in
    Lwt_io.write_line oc (Block.hash genesis))
  >> Lwt.return blockdb


let create dir p2p =
  let absolute = Filename.concat dir in
  let blockdb = initialize dir in
  BlockDB.put blockdb genesis;
  {blockdb; head;}
