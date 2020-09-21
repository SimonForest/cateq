open Parser

let token_to_string = function
  | Tset -> "Tset"
  | Tcolon -> "Tcolon"
  | Tarrow -> "Tarrow"
  | Tpareno -> "Tpareno"
  | Tparenc -> "Tparenc"
  | Teq -> "Teq"
  | Tstar -> "Tstar"
  | Tload -> "Tload"
  | Tgen -> "Tgen"
  | Tcomp x -> Printf.sprintf "Tcomp %d" x
  | Tid x -> Printf.sprintf "Tid %d" x
  | Tvar s -> Printf.sprintf "Tvar %s" s
  | Teof -> "Teof"
