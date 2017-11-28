let suites : Lwt_test.suite list = []
open Lwt_test

let test name f =
  Lwt_test.test name (fun () ->
    Lwt_log.notice("Starting Test: "^ name) >>
    let%lwt result = f () in
    Lwt_log.notice("Finished Test: "^name^"\n")
    >> Lwt.return result
  )

module StringValue = struct

  type t = string
  
  let hash t = Crypto.sha256 t

  let serialize t = t

  let deserialize t = t

end

module StringDB = Database.Make(StringValue)
open StringDB

let db1 = create "test1.db"

let value1 = "value1"

let database_tests = suite "database tests" [
  
  test "write_and_read" begin fun () ->
    put db1 value1;
    let%lwt str = get db1 (Crypto.sha256 value1) in
    Lwt.return (str = value1)
  end;
    
]

let tests = suites @ [database_tests]

let () = Lwt_test.run "all_test" suites
