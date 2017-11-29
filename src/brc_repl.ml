open ANSITerminal

type command = (string*(string list))

module CommandParser = struct 
  type t = 
    {

    }

  let load_command_file file_name = 

    

end

type t = {
  listeners:(command -> string) list
}

let header = 
  let ic = open_in "res/logo.txt" in
  let rec read_all (ic:in_channel) s = 
    try 
      let line = input_line ic  in  
      let s = s ^ line ^ "\n" in 
      read_all ic s 
    with
    | End_of_file -> 
    close_in ic; s
  in
    read_all ic ""                 

let add_listener listener repl = 
    {repl with listeners = listener::(repl.listeners)}

let handle_input () = 


let print_header () = 
  print_newline ();
  print_string [red] header;
  print_newline ()

let run () = 
  print_header ()

let () = run ()module type BRCRepl_listener = sig 
  type t 
  val on_user_input : command -> string
end