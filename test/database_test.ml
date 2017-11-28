let suites : Test.suite list = []
open Test
open Database

let test name f
  Test.test name (fun () ->
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

let db1 = Database.create "test1"

let value1 = "value1"

let database_tests = sutie "database tests" [
  
  test "write_and_read" begin fun () ->
    put db1 value1;
    let%lwt str = get db1 (Crypto.sha256 value1) in
    Lwt.return (str = value1)
  end;
    
]

let suites = suites @ [database_tests]

let () = Test.run "all_test" suites
