
open Catlang
open Parser
open Lex
open Polword

let _ =
  let in_chan = ref stdin in
  Arg.parse [] (fun s ->
      in_chan := open_in s
    ) "";
  let rec loop () =
    print_string "🐱 eq> ";
    flush stdout;
    let r = Cateqlib.handle_input (input_line !in_chan) in
    print_string r;
    loop ()
  in
  try loop () with
  | End_of_file -> ()
