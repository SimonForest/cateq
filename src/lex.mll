
{
open Parser

open Lexing

let dec_to_int =
  let str_to_list s =
    let n = String.length s in
    let rec aux i =
      if i < n then
        s.[i] :: (aux (i+1))
      else
        []
    in
    aux 0
  in
  let rec aux res = function
    [] -> res
    | a :: q -> aux (res * 10 + (int_of_char a - int_of_char '0')) q
  in
  fun s -> aux 0 (str_to_list s)
}

let d10 = ['0'-'9']
let n10 = d10+
let letter = ['a'-'z'] | ['A'-'Z']
let quote = ['\'']
let spacechar = [' ' '\t' '\n']
let compstr = "*"
let idstr = "id"
let comment = '%' [^'\n']* '\n'?

rule scan = parse
  | spacechar { scan lexbuf }
  | comment { scan lexbuf }
  | "gen" { Tgen }
  | ":" { Tcolon }
  | "=" { Teq }
  | ":=" { Tset }
  | "->" { Tarrow }
  | "*" { Tstar }
  | "(" { Tpareno }
  | ")" { Tparenc }
  | "{" { Tbrao }
  | "}" { Tbrac }
  | "," { Tcomma }
  | idstr (n10 as num) { Tid (dec_to_int num) }
  | (letter (letter | d10 | quote )*) as ident {
      match ident with
      | "makkai" -> Tfunident "makkai"
      | "henry" -> Tfunident "henry"
      | _ -> Tvar ident }
  | compstr (n10 as num) { Tcomp (dec_to_int num) }
  | eof { Teof }
