open Lib.Precat

module Z = Map.Make (struct type t = gen_st let compare g1 g2 = compare g1.gen g2.gen end)

(** Z-modules functions. *)

let mod_add_comp a (k,v) =
  let old_v =
    try Z.find k a with Not_found -> 0 in
  Z.add k (old_v + v) a

let mod_add a b =
  let l = Z.bindings b in
  List.fold_left mod_add_comp a l

let mod_sub_comp a (k,v) =
  let old_v =
    try Z.find k a with Not_found -> 0 in
  Z.add k (old_v - v) a

let mod_sub a b =
  let l = Z.bindings b in
  List.fold_left mod_sub_comp a l

(** Delta or Henry function. *)
let rec delta c =
  delta_p (c_dim c) c

and delta_p cdim c = match c with
  | VoidCell -> Z.empty
  | Id c' -> delta c'
  | Comp l ->
    let temp = l
               |> List.map (delta_w_p {n_gen = cdim; n_topw = cdim - 1})
               |> List.fold_left mod_add Z.empty
    in
    let res = if cdim = 0 then
        temp
      else
        match l with
        | [] -> failwith "0-length cell"
        | a :: q ->
          let subterm =
            q
            |> List.map (fun w -> delta_p (cdim - 1) (w_src w))
            |> List.fold_left mod_add Z.empty
          in
          mod_sub temp subterm
    in
    res

and delta_w w =
  let dim = w_gen_dim w in
  let wdim = {n_gen = dim ; n_topw = dim - 1} in
  delta_w_p wdim w

and delta_w_p wdim w = match w with
  | Gen g -> Z.singleton g 1
  | Whisk (l,iw,r) ->
    let res_iw = delta_w_p (wdim_wdec wdim) iw in
    let res_l = delta l in
    let res_r = delta r in
    let comp_dim = wdim.n_topw - 1 in
    res_iw
    |> mod_add res_l
    |> mod_add res_r
    |> (fun z -> mod_sub z (delta @@ c_tgt_p comp_dim l))
    |> (fun z -> mod_sub z (delta @@ c_src_p comp_dim r))

(** Makkai function. *)
let rec makkai c =
  let memo = ref Z.empty in
  let rec makkai_gen g =
    try Z.find g !memo with
    | Not_found ->
      begin
        let cv_src = delta_to_makkai @@ delta g.src
        and cv_tgt = delta_to_makkai @@ delta g.tgt
        in
        let res = Z.singleton g 1
                  |> mod_add cv_src
                  |> mod_add cv_tgt
        in
        memo := Z.add g res !memo;
        res
      end
  and delta_to_makkai m =
    let bdgs = Z.bindings m in
    List.fold_left
      (fun accum (k,v) ->
         let converted_coords = makkai_gen k in
         mod_add accum
           (Z.mapi (fun _ x -> x * v) converted_coords)
      )
      Z.empty
      bdgs
  in
  delta_to_makkai (delta c)

(** Printing utilities. *)
let mod_printer f fnames m =
  let bdgs = Z.bindings m in
  Format.fprintf f "[ ";
  match bdgs with
  | [] -> ()
  | (k,v) :: q ->
    begin
      Format.fprintf f "%s:%d" (str_from_gen fnames k.gen) v;
      List.iter
        (fun (k,v) ->
           Format.fprintf f ", %s:%d" (str_from_gen fnames k.gen) v)
        q
    end;
  Format.fprintf f " ]"


