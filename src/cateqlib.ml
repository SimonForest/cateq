
open Catlang
open Parser
open Lex
open Polword

exception ParserError
exception Stop

let renv = ref (new_exenv ())

let rec handle_input s =
  try
    let lex = Lexing.from_string s in
    let opt = try op scan lex
      with
      | _ -> raise ParserError in
    match opt with
    | Some op ->
      let (nenv,res) = exec_op !renv op in
      renv := nenv ;
      (
        match res with
        | None -> "-\n"
        | Some s -> s ^ "\n"
      )
    | None -> ""
  with
  | Invalid_argument str -> Printf.sprintf "Error: %s\n" str
  | ParserError -> Printf.sprintf "Syntax error\n"
