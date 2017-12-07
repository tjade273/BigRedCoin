open OUnit2
open Brc_repl

let command_parser = CommandParserImpl.from_command_file "res/commands.json"

let command_tests =
  "command tests" >::: [
    "help_test" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "help") in
                      assert_bool "" (match command with | Some _ -> true | None -> false));
    "get_block" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "get_block xfcddefmgc458vr") in
                      assert_bool "" (match command with | Some _ -> true | None -> false));
    "send" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "send 42 xfcddefmgc458vr") in
                 assert_bool "" (match command with | Some _ -> true | None -> false));
    "balance" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "balance") in
                    assert_bool "" (match command with | Some _ -> true | None -> false));
    "help_test_fail" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "help 42") in
                      assert_bool "" (match command with | Some _ -> false | None -> true));
    "get_block_fail" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "get_block") in
                      assert_bool "" (match command with | Some _ -> false | None -> true));
    "send_fail" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "send AA xfcddefmgc458vr") in
                 assert_bool "" (match command with | Some _ -> false | None -> true));
    "balance_fail" >:: (fun _ -> let command = (CommandParserImpl.parse command_parser "balance 777 KKl") in
                    assert_bool "" (match command with | Some _ -> false | None -> true));
  ]

 let tests = "Repl Tests" >::: [command_tests]
