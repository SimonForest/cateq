
(** Operations on precategories. *)

(* Conceptions issues to solve :

* the dimension of whiskers is not well-behaved regarding 0 and 1-cells.
In general, in the definition of a cell, we would like cells to have dimension (n,n-1). There are two solutions :
** either define whiskers without 0-cells. Then the dimension of 1-cells would be the same as the dimension of 0-cells
** or define whiskers with 0-cells. It makes dimension coherent but add useless information and makes the use of whiskers more complicated

The first solution was adopted.

* the definition of generators as string * (cell * cell) needs a special cell of dimension -1. Solutions :
** either use Comp []
** or use a new constructor VoidCell

The last solution was adopted.

* the exchangers are *not* defined by their source

** it is almost true except for pair of generators with 0-length source and target. In this case, it should be precised if the rotation is to be clockwise or anticlockwise
** in particular, there can be up to two possible exchangers for a pair of composable whiskers
** at the moment

 *)

(*
type ('a,'b) tlist =
  | Nil of 'a
  | Cons of ('b * ('a,'b) list)
 *)

open Format

(** A generator. *)

type gen = int (* of dimension n *)

type wdim = { n_gen : int ; n_topw : int }

(** A cell. *)

type 'a p_cell = (* of dimension n *)
  | Comp of 'a p_whisk list
  (* non-empty list of (n,n-1) whiskers *)
  | Id of 'a p_cell
  (* (n-1)-cell *)
  | VoidCell
(* the unique cell of dimension -1, should only appear in gen type for 0-cells *)

(** A whisker. *)

and 'a p_whisk = (* of dimension n m, where n is the dimension of the
                 inner cell and m the dimension of the outter cells *)
  | Gen of 'a
  | Whisk of 'a p_cell * 'a p_whisk * 'a p_cell

type gen_st = { gen : gen ; src : gen_st p_cell ; tgt : gen_st p_cell }

type cell = gen_st p_cell
type whisk = gen_st p_whisk

type s_cell = gen p_cell
type s_whisk = gen p_whisk

(** Source of a generator. *)
let g_src g = g.src

(** Target of a generator. *)
let g_tgt g = g.tgt

(* let g_name g = ??? *)

(* l *(m-1) r where l,r are of dim m *)
let c_concat l r = match l,r with
  | VoidCell,_ -> raise @@ Invalid_argument "l is voidcell"
  | _,VoidCell -> raise @@ Invalid_argument "r is voidcell"
  | Id _,_ -> r
  | _,Id _ -> l
  | Comp l',Comp r' -> Comp (l' @ r')

(** Dimension of a cell. returns [-1; inf[ *)
let rec c_dim = function
  | Id c -> c_dim c + 1
  | Comp l -> w_gen_dim (List.hd l)
  | VoidCell -> -1

and w_gen_dim = function
  | Gen g -> g_dim g
  | Whisk (_,w,_) -> w_gen_dim w

and w_whisk_dim = function
  | Gen g -> 0
  | Whisk (_,w,_) ->
     (* can also be computed as the dimension of the left/right whisker *)
     1 + w_whisk_dim w

and g_dim g = c_dim (g_src g) + 1


and comp c d =
  (* c of dim n, d of dim m, returns cell of dim max(n,m) *)
  (* assert (c_tgt c = c_src d); *) (* to do this is incorrect *)
  let dimc = c_dim c and dimd = c_dim d in
  if dimc < dimd then
    match d with
    | VoidCell -> failwith "voidcell"
    | Id d' -> Id (comp c d')
    | Comp l -> Comp (List.map (function u -> w_l_comp c u) l)
  else if dimc > dimd then
    match c with
    | VoidCell -> failwith "voidcell"
    | Id c' -> Id (comp c' d)
    | Comp l -> Comp (List.map (function u -> w_r_comp u d) l)
  else
    match (c,d) with
    | (VoidCell, _) -> failwith "voidcell"
    | (_, VoidCell) -> failwith "voidcell"
    | (Id _, _) -> d
    | (_, Id _) -> c
    | (Comp lc, Comp ld) -> Comp (lc @ ld)

and w_l_comp c w =
  (* c of dim n, w of dim (m,p) with m > p >= n > 0, returns whisker of dim (m,p) *)
  let dimc = c_dim c
  and whisk_dimw = w_whisk_dim w in
  match w with
  | Gen g -> failwith "Invalid whisker in w_l_comp"
  | Whisk (left_w,inner_w,right_w) ->
     if dimc = whisk_dimw then Whisk (comp c left_w, inner_w, right_w)
     else Whisk (comp c left_w, w_l_comp c inner_w, comp c right_w)

and w_r_comp w c =
  (* c of dim n, w of dim (m,p) with m > p >= n > 0, returns whisker of dim (m,p) *)
  let dimc = c_dim c and whisk_dimw = w_whisk_dim w in
  match w with
  | Gen g -> failwith "Invalid whisker in w_r_comp"
  | Whisk (left_w,inner_w,right_w) ->
     if dimc = whisk_dimw then Whisk (left_w, inner_w, comp right_w c)
     else Whisk (comp left_w c, w_r_comp inner_w c, comp right_w c)


and comp3 c d e = comp c (comp d e)

and w_src = function
  | Gen g -> g_src g
  | Whisk (c,w,d) -> comp3 c (w_src w) d

and w_tgt = function
  | Gen g -> g_tgt g
  | Whisk (c,w,d) -> comp3 c (w_tgt w) d

and c_src = function
  | Id c -> c
  | Comp l -> w_src (List.hd l)
  | VoidCell -> failwith "source of voidcell"

(** Target of a cell. *)
and c_tgt = function
  | Id c -> c
  | Comp l -> w_tgt (List.fold_left (function a -> function b -> b) (List.hd l) (List.tl l))
  | VoidCell -> failwith "target of voidcell"

and c_src_relp p c =
  if p = 0 then c
  else c_src_relp (p-1) (c_src c)

and c_src_p p c =
  let dimc = c_dim c in
  assert (p <= dimc);
  c_src_relp (dimc - p) c

(* TODO: put the next two outside the mutual recursion *)
and c_tgt_relp p c =
  if p = 0 then c
  else c_tgt_relp (p-1) (c_tgt c)
and c_tgt_p p c =
  let dimc = c_dim c in
  assert (p <= dimc);
  c_tgt_relp (dimc - p) c

and w_src_p p w = c_src_p p (Comp [w])

and w_tgt_p p w = c_tgt_p p (Comp [w])

(* TODO: put the next two outside the mutual recursion *)

and w_src_relp p w =
  if p = 0 then failwith "TODO"
  else
    c_src_relp (p-1) @@ w_src w

and w_tgt_relp p w =
  if p = 0 then failwith "TODO"
  else
    c_tgt_relp (p-1) @@ w_tgt w

(** big assert function for cells. *)
and c_test = function
  | VoidCell -> ()
  | Id c ->
     assert (c <> VoidCell);
     c_test c
  | Comp l ->
     assert (l <> []);
     List.iter w_test l;
     let dims = List.map (function u -> (w_gen_dim u, w_whisk_dim u)) l in
     let (x,y) = List.hd dims in
     assert (y = max (x - 1) 0 && x >= 0);
     if List.length l > 1 then assert (x > 0);
     List.iter (function (a,b) -> assert (a = x && b = y)) dims;
     let _ = List.fold_left (function prev -> function actual -> assert (w_tgt prev = w_src actual); actual) (List.hd l) (List.tl l) in
     ()

(** big assert function for whiskers. *)
and w_test = function
  | Gen g -> ()
  | Whisk (l,w,r) ->
     w_test w;
     c_test l;
     c_test r;
     let n = w_gen_dim w
     and m = w_whisk_dim w in
     assert (m <= n-2); (* if a whisk is of dim (n,m), we should have m <= n-1) *)
     (* at this point, we know that n >= 2 and m >= 0*)
     let ldim = c_dim l and rdim = c_dim r in
     assert (ldim = m+1 && rdim = m+1);
     assert (c_tgt l = w_src_p m w && w_tgt_p m w = c_src r)

let wdim_wdec wdim =
  { wdim with n_topw = wdim.n_topw - 1 }
let rec w_src_p_opt p wdim w =
  match () with
  | _ when p < wdim.n_topw ->
    let Whisk (l,_,_) = w in
    c_src_relp (wdim.n_topw - p) l
  | _ when wdim.n_topw > 0 ->
    let Whisk (l,iw,r) = w in
    comp3 l (w_src_p_opt p (wdim_wdec wdim) iw) r
  | _ -> let Gen g = w in
    if p = wdim.n_gen then
      failwith "TODO"
    else if p < wdim.n_gen then
      c_src_relp (wdim.n_gen - p - 1) g.src
    else
      failwith "TODO"

let rec w_tgt_p_opt p wdim w =
  match () with
  | _ when p < wdim.n_topw ->
    let Whisk (_,_,r) = w in
    c_src_relp (wdim.n_topw - p) r
  | _ when p < wdim.n_topw ->
    let Whisk (l,iw,r) = w in
    comp3 l (w_tgt_p_opt p (wdim_wdec wdim) iw) r
  | _ -> let Gen g = w in
    if p = wdim.n_gen then
      failwith "TODO"
    else if p < wdim.n_gen then
      c_tgt_relp (wdim.n_gen - p - 1) g.tgt
    else
      failwith "TODO"
let rec w_get_gen w = match w with
  | Gen g -> g
  | Whisk (_,w,_) -> w_get_gen w

let rec w_extract_gen = function
  | Gen g -> g
  | Whisk (l,w,r) -> w_extract_gen w

(** Cells creation*)

let gen_to_whisk g =
  let dimg = g_dim g in
  let rec gen_to_whisk' = function
    | 0 -> Gen g
    | p -> Whisk (Id (c_src_p (p-1) (g_src g)), gen_to_whisk' (p-1), Id (c_tgt_p (p-1) (g_tgt g)))
  in gen_to_whisk' (max (dimg - 1) 0)

let void_cell = VoidCell

let gen_to_cell g =
  let c = Comp [gen_to_whisk g] in
  c_test c; c

let create_gen gen src tgt =
  let dims = c_dim src and dimt = c_dim tgt in
  assert (dims = dimt);
  if dims >= 1 then assert (c_src src = c_src tgt && c_tgt src = c_tgt tgt);
  { gen = gen ; src = src ; tgt = tgt }

let create_gen_cell gen src tgt =
  let genst = create_gen gen src tgt in
  (genst, gen_to_cell genst)

let create_cell str source target =
  gen_to_cell (create_gen str source target)

let compose_cells c1 c2 =
  let dim1 = c_dim c1 and dim2 = c_dim c2 in
  assert (dim1 > 0 && dim2 > 0);
  let dimcomp = (min dim1 dim2) - 1 in
  assert (c_tgt_p dimcomp c1 = c_src_p dimcomp c2);
  let c = comp c1 c2 in
  c_test c; c

let compose_lcells l =
  let a::q = l in
  List.fold_left (fun r c -> compose_cells r c) a q

let id_cell c = Id c

let id_cell_p p c =
  let n = c_dim c in
  if p < n then
    raise @@ Invalid_argument "p < n" ;
  let rec aux u = function
    | 0 -> u
    | n -> aux (id_cell u) (n-1)
  in
  aux c (p - n)

let size_cell c =
  match c with
  | VoidCell -> failwith "voidcell"
  | Id _ -> 0
  | Comp l -> List.length l

(** Printers. *)

let str_from_gen fnames gen =
  match fnames with
  | None -> string_of_int gen
  | Some f -> f gen


let whisk_printer f fnames w =
  let g = w_get_gen w in
  let s = str_from_gen fnames g.gen in
  Format.fprintf f "%s" s

let whisk_l_size = function
  | Gen g -> 0
  | Whisk (l,_,_) -> size_cell l

let cell_printer f fnames c =
  let topstr_w w =
    let g = w_get_gen w in
    if g_dim g < 2 then
      str_from_gen fnames g.gen
    else
      Format.sprintf "%d:%s" (whisk_l_size w) (str_from_gen fnames g.gen)
  in

  let top_format_of_cell c =
    match c with
    | Comp l ->
       Format.fprintf f "%s" (topstr_w (List.hd l));
       List.iter (fun u -> Format.fprintf f " * %s" (topstr_w u)) (List.tl l)
    | Id c ->
       Format.fprintf f "id"
    | VoidCell -> Format.fprintf f "voidcell"
  in
  top_format_of_cell c;
  Format.fprintf f "@.";
  let rec aux c =
    if c_dim c = 0 then ()
    else
      let s = c_src c and t = c_tgt c in
      Format.fprintf f "from : ";
      top_format_of_cell s;
      Format.fprintf f " to : ";
      top_format_of_cell t;
      Format.fprintf f "@.";
      aux s
  in
  aux c

let string_from_cell fnames c =
  let b = Buffer.create 0 in
  let f = Format.formatter_of_buffer b in
  cell_printer f fnames c;
  Format.pp_print_flush f ();
  Buffer.contents b

let print_cell fnames c =
  Format.printf "%s" (string_from_cell fnames c);
  Format.print_flush ()

let print_whisk fnames w =
  print_string (string_from_cell fnames (Comp w))

(** In the top-level : #install_printer cell_printer *)
