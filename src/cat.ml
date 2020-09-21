open Lib
open Precat

type 'a cell_form =
  | CFGen of 'a
  | CFComp of int * 'a cell_form * 'a cell_form
  | CFId of 'a cell_form

let rec comp_p' p l diml r dimr =
  if p >= min diml dimr then
    raise @@ Invalid_argument "p >= min(diml,dimr)" ;
  if diml > dimr then
    match l with
    | Id l' -> Id (comp_p' p l' (diml - 1) r dimr)
    | Comp list -> Comp (list |> List.map (fun w -> w_r_comp_p' p w (diml - 1) r dimr))
    | VoidCell -> raise @@ Invalid_argument "l is the empty cell"
  else if diml < dimr then
    match r with
    | Id r' -> Id (comp_p' p l diml r' (dimr - 1))
    | Comp list -> Comp (list |> List.map (fun w -> w_l_comp_p' p l diml w (dimr - 1)))
    | VoidCell -> raise @@ Invalid_argument "r is the empty cell"
  else if p = diml - 1 then
    c_concat l r
  else
    let ltgt = c_tgt_p (diml - 1) l
    and rsrc = c_src_p (dimr - 1) r
    in
    c_concat (comp_p' p l diml rsrc (dimr - 1)) (comp_p' p ltgt (diml - 1) r dimr)

and w_l_comp_p' p c dimc w dimw =
  if dimc > dimw then
    raise @@ Invalid_argument "whisker extension with a cell of higher dimension" ;
  match w with
  | Gen g -> raise @@ Invalid_argument "can not extend 0-dim whisk"
  | Whisk (l,iw,r) ->
    if dimw > dimc then
      Whisk (comp_p' p c dimc l dimw, w_l_comp_p' p c dimc iw (dimw - 1),comp_p' p c dimc r dimw)
    else if p = dimc - 1 then
      Whisk (c_concat c l,iw,r)
    else
      let ctgt = c_tgt c in
      Whisk (comp_p' p c dimc l dimw,w_l_comp_p' p ctgt (dimc - 1) iw (dimw - 1),comp_p' p ctgt (dimc - 1) r dimw)

and w_r_comp_p' p w dimw c dimc =
  if dimc > dimw then
    raise @@ Invalid_argument "whisker extension with a cell of higher dimension" ;
  match w with
  | Gen g -> raise @@ Invalid_argument "can not extend 0-dim whisk"
  | Whisk (l,iw,r) ->
    if dimw > dimc then
      Whisk (comp_p' p l dimw c dimc,w_r_comp_p' p iw (dimw - 1) c dimc,comp_p' p r dimw c dimc)
    else if p = dimc - 1 then
      Whisk (l,iw,c_concat r c)
    else
      let csrc = c_src c in
      Whisk (comp_p' p l dimw csrc (dimc - 1),w_r_comp_p' p iw (dimw - 1) csrc (dimc - 1),comp_p' p r dimw c dimc)

let rec comp_p p l r =
  let diml = c_dim l in
  let dimr = c_dim r in
  comp_p' p l diml r dimr
