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

let db1location = "test1.db"

let db1 = create db1location

let value1 = "value1"

let value2 = "value2"

let database_tests = suite "database tests" [
  
  test "write_and_read" begin fun () ->
    let%lwt () = put db1 value1 in
    let%lwt str = get db1 (Crypto.sha256 value1) in
    Lwt.return (str = value1)
  end;

  test "write_and_mem" begin fun () ->
    let%lwt () = put db1 value1 in
    let%lwt res1 = mem db1 (Crypto.sha256 value1) in
    let%lwt () = remove db1 (Crypto.sha256 value1) in
    let%lwt res2 = mem db1 (Crypto.sha256 value1) in
    Lwt.return ((res1, res2) = (true, false))
  end;

  test "get_not_present" begin fun () ->
    let%lwt () = remove db1 (Crypto.sha256 value1) in  
    Lwt.catch begin
      fun () -> (
        let%lwt res = get db1 (Crypto.sha256 value1) in
        Lwt.return false
      )
    end (fun exn -> Lwt.return true)
  end;

  test "get_option" begin fun () ->
    let%lwt () = remove db1 (Crypto.sha256 value1) in
    let%lwt res1 = get_opt db1 (Crypto.sha256 value1) in
    let%lwt () = put db1 value1 in
    let%lwt res2 = get_opt db1 (Crypto.sha256 value1) in
    match res1, res2 with
    | None, Some y -> Lwt.return true
    | _, _ -> Lwt.return false
  end;

  test "make_second_database" begin fun () ->
    Lwt.catch begin
      fun () -> (
        let db2 = create db1location in
        Lwt.return false
      )
    end (fun exn -> Lwt.return true)
  end;

  test "put_two_values" begin fun () ->
    let%lwt () = remove db1 (Crypto.sha256 value1) in
    let%lwt () = remove db1 (Crypto.sha256 value2) in
    let%lwt () = put db1 value1 in
    let%lwt () = put db1 value2 in
    let%lwt res1 = mem db1 (Crypto.sha256 value1) in
    let%lwt res2 = mem db1 (Crypto.sha256 value2) in
    Lwt.return ((res1, res2) = (true, true))
    end;
]

let tests = suites @ [database_tests]

let () = Lwt_test.run "all_test" suites
