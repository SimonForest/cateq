open Lib

open Precat

open Cat

module P = Parity

(* module M = Map.Make (OrdGen) *)

type seqcl = SeqCl of int
type ctxtcl = CtxtCl of int

let fresh_cid =
  let r = ref (-1) in
  fun () -> (incr r; SeqCl !r)

let fresh_wid =
  let r = ref (-1) in
  fun () -> (incr r; CtxtCl !r)

(** The type of contexts of generator type *)
type ctxt =
  | CtxtGen of gen_st (** a $0$-context which consists in a generator *)
  | CtxtTriple of seqcl * ctxtcl * seqcl (** an $(n{+}1)$-context *)

(** The type of the $n$-sequences *)
type seq =
  | SeqNull (** the unique (-1)-sequence *)
  | SeqId of seqcl (** an empty $n$-sequence *)
  | SeqSeq of ctxtcl list (** a non-empty $n$-sequence *)

let seqcl_l = ref []

let ctxtcl_l = ref []

(* let gen_srctgt_m = ref M.empty *)

let find_seq_cl (c : seq) : seqcl option =
  try Some (fst (List.find (fun x -> List.mem c @@ snd x) !seqcl_l) ) with
  | Not_found -> None

let find_seqcl_reps (cl : seqcl) =
  try Some (snd (List.find (fun x -> fst x = cl) !seqcl_l)) with
  | Not_found -> None

let set_seq_cl (l : seq list) (cl : seqcl) =
  seqcl_l := (cl,l) :: !seqcl_l

let find_ctxt_cl (w : ctxt) : ctxtcl option =
  try Some (fst (List.find (fun x -> List.mem w @@ snd x) !ctxtcl_l) ) with
  | Not_found -> None

let find_ctxtcl_reps (cl : ctxtcl) =
  try Some (snd (List.find (fun x -> fst x = cl) !ctxtcl_l)) with
  | Not_found -> None

let set_ctxt_cl (l : ctxt list) (cl : ctxtcl) =
  ctxtcl_l := (cl,l) :: !ctxtcl_l

let get_seqcl_rep (cl : seqcl) =
  match find_seqcl_reps cl with
  | None -> raise @@ Invalid_argument "seq class does not exist"
  | Some l -> List.hd l

let get_ctxtcl_rep (cl : ctxtcl) =
  match find_ctxtcl_reps cl with
  | None -> raise @@ Invalid_argument "ctxt class does not exist"
  | Some l -> List.hd l

(* let find_gen_srctgt_cl (g : gen_st) : (seqcl * seqcl) option =
 *   M.find_opt g.gen !gen_srctgt_m
 *
 * let set_gen_srctgt_cl (g : gen_st) (src,tgt : seqcl*seqcl) =
 *   gen_srctgt_m := M.add g.gen (src,tgt) !gen_srctgt_m *)

(* Iteration on class members *)

let it_on_seqcl_reps f cl =
  match find_seqcl_reps cl with
  | Some l -> List.iter f l
  | None -> failwith "Undefined seqcl"

let map_on_seqcl_reps f cl =
  match find_seqcl_reps cl with
  | Some l -> List.map f l
  | None -> failwith "Undefined seqcl"

let it_on_ctxtcl_reps f cl =
  match find_ctxtcl_reps cl with
  | Some l -> List.iter f l
  | None -> failwith "Undefined ctxtcl"

let map_on_ctxtcl_reps f cl =
  match find_ctxtcl_reps cl with
  | Some l -> List.map f l
  | None -> failwith "Undefined ctxtcl"

(* and gen_st' = { gen : gen ; src : seqcl ; tgt : seqcl } *)
(* let rec compare_seq = fun
 *     | SeqNull SeqNull = EQ
 *     | SeqNull _ = LT
 *     | _ SeqNull = GT
 *     | (SeqSeq) SeqSeq
 *
 * module OrdCell = struct
 *   type t = cell
 * end *)

let rec seq_dim = function
  | SeqId c -> seqcl_dim c + 1
  | SeqSeq l -> ctxtcl_gen_dim (List.hd l)
  | SeqNull -> -1 (* important for g'_dim *)

and seqcl_dim cl =
  seq_dim @@ get_seqcl_rep cl

and w'_gen_dim = function
  | CtxtGen g -> g_dim g
  | CtxtTriple (_,w,_) -> ctxtcl_gen_dim w

and ctxtcl_gen_dim cl =
  w'_gen_dim @@ get_ctxtcl_rep cl

and w'_ctxt_dim = function
  | CtxtGen g -> 0
  | CtxtTriple (_,w,_) ->
     (* can also be computed as the dimension of the left/right ctxter *)
     1 + (w'_ctxt_dim @@ get_ctxtcl_rep w)

let w'_wdim w =
  { n_gen = w'_gen_dim w ; n_topw = w'_ctxt_dim w }

let c'_size = function
  | SeqNull -> 0
  | SeqId _ -> 0
  | SeqSeq l -> List.length l
(* and g'_dim g = (seqcl_dim g.src) + 1 *)

(* We try to avoid rewriting all the operations for quotiented cells and
   whiskers by using conversion back to precats instead and using src, tgt and
   comp defined for them *)
(* let rec c'_comp c d =
 *   (\* c of dim n, d of dim m, returns cell of dim max(n,m) *\)
 *   (\* assert (c_tgt c = c_src d); *\) (\* to do this is incorrect *\)
 *   let dimc = seq_dim c and dimd = seq_dim d in
 *   if dimc < dimd then
 *     match d with
 *     | CellNull -> failwith "voidcell"
 *     | CellId d' -> Id (seqcl_comp c d')
 *     | CellSeq l -> Comp (List.map (function u -> ctxtcl_l_comp c u) l)
 *   else if dimc > dimd then
 *     match c with
 *     | CellNull -> failwith "voidcell"
 *     | CellId c' -> Id (comp c' d)
 *     | CellSeq l -> Comp (List.map (function u -> w_r_comp u d) l)
 *   else
 *     match (c,d) with
 *     | (VoidCell, _) -> failwith "voidcell"
 *     | (_, VoidCell) -> failwith "voidcell"
 *     | (Id _, _) -> d
 *     | (_, Id _) -> c
 *     | (Comp lc, Comp ld) -> Comp (lc @ ld)
 *
 * and w_l_comp c w =
 *   (\* c of dim n, w of dim (m,p) with m > p >= n > 0, returns whisker of dim (m,p) *\)
 *   let dimc = c_dim c
 *   and whisk_dimw = w_whisk_dim w in
 *   match w with
 *   | Gen g -> failwith "Invalid whisker in w_l_comp"
 *   | Whisk (left_w,inner_w,right_w) ->
 *      if dimc = whisk_dimw then Whisk (comp c left_w, inner_w, right_w)
 *      else Whisk (comp c left_w, w_l_comp c inner_w, comp c right_w)
 *
 * and w_r_comp w c =
 *   (\* c of dim n, w of dim (m,p) with m > p >= n > 0, returns whisker of dim (m,p) *\)
 *   let dimc = c_dim c and whisk_dimw = w_whisk_dim w in
 *   match w with
 *   | Gen g -> failwith "Invalid whisker in w_r_comp"
 *   | Whisk (left_w,inner_w,right_w) ->
 *      if dimc = whisk_dimw then Whisk (left_w, inner_w, comp right_w c)
 *      else Whisk (comp left_w c, w_r_comp inner_w c, comp right_w c)
 *
 *
 * and comp3 c d e = comp c (comp d e)
 *
 * and w_src = function
 *   | Gen g -> g_src g
 *   | Whisk (c,w,d) -> comp3 c (w_src w) d
 *
 * and w_tgt = function
 *   | Gen g -> g_tgt g
 *   | Whisk (c,w,d) -> comp3 c (w_tgt w) d
 *
 * and c_src = function
 *   | Id c -> c
 *   | Comp l -> w_src (List.hd l)
 *   | VoidCell -> failwith "source of voidcell"
 *
 * (\** Target of a cell. *\)
 * and c_tgt = function
 *   | Id c -> c
 *   | Comp l -> w_tgt (List.fold_left (function a -> function b -> b) (List.hd l) (List.tl l))
 *   | VoidCell -> failwith "target of voidcell"
 *
 * and c_src_p p c =
 *   let dimc = c_dim c in
 *   assert (p <= dimc);
 *   let rec c_src_p' p c =
 *     if p = 0 then c
 *     else c_src_p' (p-1) (c_src c)
 *   in c_src_p' (dimc - p) c
 *
 * and c_tgt_p p c =
 *   let dimc = c_dim c in
 *   assert (p <= dimc);
 *   let rec c_tgt_p' p c =
 *     if p = 0 then c
 *     else c_tgt_p' (p-1) (c_tgt c)
 *   in c_tgt_p' (dimc - p) c
 *
 * and w_src_p p w = c_src_p p (Comp [w])
 *
 * and w_tgt_p p w = c_tgt_p p (Comp [w]) *)

let size_seq = function
  | SeqNull -> 0
  | SeqId _ -> 0
  | SeqSeq l -> List.length l

let size_seqcl cl = size_seq @@ get_seqcl_rep cl

(* (n-1) composition of n-c', a.k.a concatenation *)
let c'_concat c1 c2 =
  match (c1,c2) with
  | SeqNull,_ -> failwith ""
  | _,SeqNull -> failwith ""
  | SeqId _,_ -> c2
  | _,SeqId _ -> c1
  | SeqSeq l1, SeqSeq l2 -> SeqSeq (l1 @ l2)

let rec l_prefix_exact n l =
  match n with
  | 0 -> []
  | k ->
    match l with
    | a :: q -> a :: (l_prefix_exact (n-1) q)
    | _ -> raise @@ Invalid_argument "n too big relative to l"

let rec l_suffix_exact n l =
  let size = List.length l in
  if size < n then raise @@ Invalid_argument "n too big relative to l" ;
  let rec aux i s =
    match i with
    | 0 -> s
    | i ->
      match s with
      | a :: q -> aux (i-1) q
      | _ -> failwith "???"
  in
  aux (size - n) l

let rec l_init l =
  match l with
  [] -> raise @@ Invalid_argument "empty list"
  | [ a ] -> []
  | a :: q -> a :: (l_init q)

let rec l_last l =
  match l with
  [] -> raise @@ Invalid_argument "empty list"
  | [ a ] -> a
  | a :: q -> l_last q

let c'_prefix n c =
  match n with
  | 0 -> None
  | k ->
    match c with
    | SeqSeq l ->
      let l' = l_prefix_exact n l in
      Some (SeqSeq l')
    | _ -> raise @@ Invalid_argument "c has no prefix > 0"

let c'_suffix n c =
  match n with
  | 0 -> None
  | k ->
    match c with
    | SeqSeq l ->
      let l' = l_suffix_exact n l in
      Some (SeqSeq l')
    | _ -> raise @@ Invalid_argument "c has no suffix > 0"

(* Memoization structures *)
(* TODO more efficient implementation of memoization *)
(* TODO put the global variables in a context *)


(* Exception *)

exception Early_stop

(* Functions to get the class *)

(* TODO include dimension as argument *)
(* TODO solve inconsistent namings

   - inner calls should use get seqcl_memo on the one hand and
   get_gen_srctgt_cl on the other hand *)

(* TODO check call conventions : *, *_memo, *' ? *)
(* let get_gen_srctgt_cl' (g : gen_st) : (seqcl * seqcl) = failwith "todo"
 *
 * let get_gen_srctgt_cl (g : gen_st) : (seqcl * seqcl) =
 *   match find_gen_srctgt_cl g with
 *   | Some r -> r
 *   | None -> let res = get_gen_srctgt_cl' g in set_gen_srctgt_cl g res ; res *)

module Sequence =
struct
  type t = ctxtcl list
  let compare = compare
end

module SequenceSet = Set.Make (Sequence)

module Context =
struct
  type t = ctxt
  let compare = compare
end

module ContextSet = Set.Make (Context)

let rec get_seqcl' (c : seq) : seqcl =
  match c with
  | SeqSeq l ->
    let dim = seq_dim c in
    let rec aux aldone = function
      | [] -> aldone
      | curr :: nexts ->
        if SequenceSet.mem curr aldone then
          aux aldone nexts
        else
          let ngbrs = SequenceSet.elements @@ cpt_wl_ngbrs curr dim in
          let nodes' = ngbrs @ nexts in
          aux (SequenceSet.add curr aldone) nodes'
    in
    let clreps = List.map (fun x -> SeqSeq x) @@ SequenceSet.elements @@ aux SequenceSet.empty [l] in
    let cl = fresh_cid () in
    set_seq_cl clreps cl ;
    cl
  | _ ->
    let res = [c] in
    let cl = fresh_cid () in
    set_seq_cl res cl ;
    cl

and cpt_wl_ngbrs wl dim =
  let res = ref SequenceSet.empty in
  match wl with
  | [] -> failwith "empty whisker list"
  | [w] -> !res
  | a::b::q ->
    let w_dim = { n_gen = dim ; n_topw = dim - 1 } in
    let rec aux l a b r =
      let exchs = cpt_exch_btw_ctxtcl a w_dim b w_dim in
      exchs |> List.iter (fun (c,d) ->
          let temp = l @ ([c ; d] @ r) in
          res := SequenceSet.add temp !res
        ) ;
      match r with
      | c :: r' -> aux (l @ [a]) b c r'
      | _ -> ()
    in
    aux [] a b q ; !res

and get_ctxtcl' (w : ctxt) : ctxtcl =
  match w with
  | CtxtGen g -> let res = [w] in
    let cl = fresh_wid () in
    set_ctxt_cl res cl ;
    cl
  | CtxtTriple _ ->
    let wdim = w'_wdim w in
    let rec aux aldone = function
      | [] -> aldone
      | curr :: nexts ->
        if ContextSet.mem curr aldone then
          aux aldone nexts
        else
          let ngbrs = ContextSet.elements @@ cpt_w'_ngbrs curr wdim in
          aux (ContextSet.add curr aldone) (ngbrs @ nexts)
    in
    let ngbrs = ContextSet.elements @@ aux (ContextSet.empty) [w] in
    let wid = fresh_wid () in
    set_ctxt_cl ngbrs wid ;
    wid


and cpt_w'_ngbrs w wdim =
  match w with
  | CtxtGen g -> ContextSet.empty
  | CtxtTriple (l,ictxtcl,r) ->
    let res = ref ContextSet.empty in
    let common_rrep = get_seqcl_rep r in
    l |> it_on_seqcl_reps (
      fun lrep ->
        match lrep with
        | SeqNull -> ()
        | SeqId _ -> ()
        | SeqSeq list ->
          let firsts = l_init list in
          let last = l_last list in
          let last_wdim = { n_gen = wdim.n_topw ; n_topw = wdim.n_topw - 1 }
          and ictxtcl_wdim = { wdim with n_topw = wdim.n_topw - 1 }
          in
          let exchs = cpt_exch_btw_ctxtcl last last_wdim ictxtcl ictxtcl_wdim in
          exchs |> List.iter (
            fun (new_ictxtcl,new_r_first) ->
              let new_lrep = match firsts with
                | _ :: _ -> SeqSeq firsts
                | [] -> let temp = ctxtcl_src_relp (ictxtcl_wdim.n_gen - ictxtcl_wdim.n_topw) new_ictxtcl in
                  SeqId temp
              in
              let new_l = get_seqcl new_lrep in
              let new_r =
                let new_rrep = c'_concat (SeqSeq [new_r_first]) common_rrep in
                get_seqcl new_rrep
              in
              let ngbr = CtxtTriple (new_l,new_ictxtcl,new_r) in
              res := ContextSet.add ngbr !res
          )
    ) ;
    let common_lrep = get_seqcl_rep l in
    r |> it_on_seqcl_reps (
      fun rrep ->
        match rrep with
        | SeqNull -> ()
        | SeqId _ -> ()
        | SeqSeq list ->
          let first = List.hd list in
          let lasts = List.tl list in
          let first_wdim = { n_gen = wdim.n_topw ; n_topw = wdim.n_topw - 1 }
          and ictxtcl_wdim = { wdim with n_topw = wdim.n_topw - 1 }
          in
          let exchs = cpt_exch_btw_ctxtcl ictxtcl ictxtcl_wdim first first_wdim in
          exchs |> List.iter (
            fun (new_l_last,new_ictxtcl) ->
              let new_rrep = match lasts with
                | _ :: _ -> SeqSeq lasts
                | [] -> let temp = ctxtcl_tgt_relp (ictxtcl_wdim.n_gen - ictxtcl_wdim.n_topw) new_ictxtcl in
                  SeqId temp
              in
              let new_r = get_seqcl new_rrep in
              let new_l =
                let new_lrep = c'_concat common_lrep (SeqSeq [new_l_last]) in
                get_seqcl new_lrep
              in
              let ngbr = CtxtTriple (new_l,new_ictxtcl,new_r) in
              res := ContextSet.add ngbr !res
          )
    ) ;
    !res

and get_seqcl (c : seq) : seqcl =
  match find_seq_cl c with
  | Some cl -> cl
  | None -> let cl = get_seqcl' c in cl

and get_ctxtcl (w : ctxt) : ctxtcl =
  match find_ctxt_cl w with
  | Some cl -> cl
  | None -> let cl = get_ctxtcl' w in cl

(* x calls x' with dimension computed *)

and convert_pcell_to_seqcl = function
  VoidCell -> get_seqcl SeqNull
  | Id a -> get_seqcl (SeqId (convert_pcell_to_seqcl a))
  | Comp l -> get_seqcl (SeqSeq (List.map convert_whisk_to_ctxtcl l))

and convert_whisk_to_ctxtcl = function
  Gen g -> get_ctxtcl (CtxtGen g)
  | Whisk (l,w,r) -> let l' = convert_pcell_to_seqcl l
    and r' = convert_pcell_to_seqcl r
    and w' = convert_whisk_to_ctxtcl w in
    get_ctxtcl (CtxtTriple (l',w',r'))

and convert_ctxtcl_to_whisk cl =
  let h = get_ctxtcl_rep cl in
  match h with
  | CtxtGen g -> Gen g
  | CtxtTriple (l,w,r) ->
    let lrep = convert_seqcl_to_pcell l
    and rrep = convert_seqcl_to_pcell r
    and wrep = convert_ctxtcl_to_whisk w
    in
    Whisk (lrep,wrep,rrep)

and convert_seqcl_to_pcell cl =
  let h = get_seqcl_rep cl in
  match h with
  | SeqNull -> VoidCell
  | SeqId u -> Id (convert_seqcl_to_pcell u)
  | SeqSeq l -> Comp (List.map convert_ctxtcl_to_whisk l)

and ctxtcl_src_relp p cl =
  let w = convert_ctxtcl_to_whisk cl in
  let c = w_src_relp p w in
  convert_pcell_to_seqcl c

and ctxtcl_tgt_relp p cl =
  let w = convert_ctxtcl_to_whisk cl in
  let c = w_tgt_relp p w in
  convert_pcell_to_seqcl c

(* Exchanges *)

(** clockwise and counterclockwise exchange between two whiskers *)

(* hi is the difference between the gen dim and the top whisker dim *)

(* compute clockwise exchange between whiskers *)
(* TODO: make a not-' version where w1 and w2 are quotiented whiskers *)
and cpt_cw_exch_btw_ctxtcl (ctxtcl1 : ctxtcl) wdim1 (ctxtcl2 : ctxtcl) wdim2 =
  ctxtcl1 |> map_on_ctxtcl_reps (
    fun w1 ->
      ctxtcl2 |> map_on_ctxtcl_reps (
        fun w2 ->
          cpt_cw_exch_btw_ctxt w1 wdim1 w2 wdim2
      ) |> List.concat
  ) |> List.concat
and cpt_ccw_exch_btw_ctxt w1 wdim1 w2 wdim2 =
  try
    match (w1,w2) with
      ((CtxtGen g1),(CtxtGen g2)) -> []
    | (CtxtTriple (l1,ctxtcl1,r1)),(CtxtTriple (l2,ctxtcl2,r2)) ->
      let res = ref [] in
      let n_l1 = size_seqcl l1
      and n_r1 = size_seqcl r1
      and n_l2 = size_seqcl l2
      and n_r2 = size_seqcl r2
      (* and ctxtcl1rep = List.hd ctxtcl1
       * and ctxtcl2rep = List.hd ctxtcl2 *)
      in
      let ctxtcl1src = ctxtcl_src_relp (wdim1.n_gen - wdim1.n_topw) ctxtcl1
      and ctxtcl1tgt = ctxtcl_tgt_relp (wdim1.n_gen - wdim1.n_topw) ctxtcl1
      and ctxtcl2src = ctxtcl_src_relp (wdim2.n_gen - wdim2.n_topw) ctxtcl2
      and ctxtcl2tgt = ctxtcl_tgt_relp (wdim2.n_gen - wdim2.n_topw) ctxtcl2
      in
      let n_ctxtcl1tgt = size_seqcl ctxtcl1tgt
      and n_ctxtcl2src = size_seqcl ctxtcl2src
      in
      if n_l1 + n_ctxtcl1tgt > n_l2 then raise Early_stop ;
      let l1rep = get_seqcl_rep l1
      and r2rep = get_seqcl_rep r2
      in
      let l1_ctxtcl1tgt_comp_rep = c'_concat l1rep @@ get_seqcl_rep ctxtcl1tgt
      and l1_ctxtcl1src_comp_rep = c'_concat l1rep @@ get_seqcl_rep ctxtcl1src
      and ctxtcl2src_r2_comp_rep = c'_concat (get_seqcl_rep ctxtcl2src) r2rep
      and ctxtcl2tgt_r2_comp_rep = c'_concat (get_seqcl_rep ctxtcl2tgt) r2rep
      in
      l2 |> it_on_seqcl_reps (
        fun l2rep ->
          try
            let pl2rep = c'_prefix (n_l1 + n_ctxtcl1tgt) l2rep in
            if (match pl2rep with None -> false
                                | Some c -> c <> l1_ctxtcl1tgt_comp_rep)
            then raise Early_stop ;
            let n_common = n_l2 - n_l1 - n_ctxtcl1tgt in
            let sl2rep = c'_suffix n_common l2rep in
            r1 |> it_on_seqcl_reps (
              fun r1rep ->
                try
                  let pr1rep = c'_prefix n_common r1rep in
                  (match sl2rep,pr1rep with
                  | None,None -> ()
                  | Some (SeqId _),None -> ()
                  | None, Some (SeqId _) -> ()
                  | Some (SeqSeq u), Some (SeqSeq v) -> if u <> v then raise Early_stop
                  | _,_ -> failwith "???") ;
                  let sr1rep = c'_suffix (n_r1 - n_common) r1rep in
                  if (match sr1rep with None -> false
                                      | Some c -> c <> ctxtcl2src_r2_comp_rep)
                  then raise Early_stop ;
                  let new_l1_rep = match sl2rep with None -> l1_ctxtcl1src_comp_rep
                                                   | Some c -> c'_concat l1_ctxtcl1src_comp_rep c
                  and new_r2_rep = match sl2rep with None -> ctxtcl2tgt_r2_comp_rep
                                                   | Some c -> c'_concat c ctxtcl2tgt_r2_comp_rep
                  in
                  let new_l1 = get_seqcl new_l1_rep
                  and new_r2 = get_seqcl new_r2_rep
                  in
                  let new_w1 = get_ctxtcl @@ CtxtTriple (new_l1,ctxtcl2,r2)
                  and new_w2 = get_ctxtcl @@ CtxtTriple (l1,ctxtcl1,new_r2)
                  in
                  res := (new_w1,new_w2) :: !res
                with Early_stop -> ()
            )
          with
            Early_stop -> ()
      ) ;
      !res
    | _,_ -> failwith "Whiskers do not have the same shape"
  with
    Early_stop -> []

(* compute counter-clockwise exchange between whiskers *)
(* TODO: make a not-' version where w1 and w2 are quotiented whiskers *)
and cpt_ccw_exch_btw_ctxtcl ctxtcl1 wdim1 ctxtcl2 wdim2 =
  ctxtcl1 |> map_on_ctxtcl_reps (
    fun w1 ->
      ctxtcl2 |> map_on_ctxtcl_reps (
        fun w2 ->
          cpt_ccw_exch_btw_ctxt w1 wdim1 w2 wdim2
      ) |> List.concat
  ) |> List.concat
and cpt_cw_exch_btw_ctxt w1 wdim1 w2 wdim2 =
  try
    match (w1,w2) with
      ((CtxtGen g1),(CtxtGen g2)) -> []
    | (CtxtTriple (l1,ctxtcl1,r1)),(CtxtTriple (l2,ctxtcl2,r2)) ->
      let res = ref [] in
      let n_l1 = size_seqcl l1
      and n_r1 = size_seqcl r1
      and n_l2 = size_seqcl l2
      and n_r2 = size_seqcl r2
      (* and ctxtcl1rep = List.hd ctxtcl1
       * and ctxtcl2rep = List.hd ctxtcl2 *)
      in
      let ctxtcl1src = ctxtcl_src_relp (wdim1.n_gen - wdim1.n_topw) ctxtcl1
      and ctxtcl1tgt = ctxtcl_tgt_relp (wdim1.n_gen - wdim1.n_topw) ctxtcl1
      and ctxtcl2src = ctxtcl_src_relp (wdim2.n_gen - wdim2.n_topw) ctxtcl2
      and ctxtcl2tgt = ctxtcl_tgt_relp (wdim2.n_gen - wdim2.n_topw) ctxtcl2
      in
      let n_ctxtcl1tgt = size_seqcl ctxtcl1tgt
      and n_ctxtcl2src = size_seqcl ctxtcl2src
      in
      if n_l2 + n_ctxtcl2src > n_l1 then raise Early_stop ;
      let l2rep = get_seqcl_rep l2
      and r1rep = get_seqcl_rep r1
      in
      let l2_ctxtcl2tgt_comp_rep = c'_concat l2rep @@ get_seqcl_rep ctxtcl2tgt
      and l2_ctxtcl2src_comp_rep = c'_concat l2rep @@ get_seqcl_rep ctxtcl2src
      and ctxtcl1src_r1_comp_rep = c'_concat (get_seqcl_rep ctxtcl1src) r1rep
      and ctxtcl1tgt_r1_comp_rep = c'_concat (get_seqcl_rep ctxtcl1tgt) r1rep
      in
      l1 |> it_on_seqcl_reps (
        fun l1rep ->
          try
            let pl1rep = c'_prefix (n_l2 + n_ctxtcl2src) l1rep in
            if (match pl1rep with None -> false
                                | Some c -> c <> l2_ctxtcl2src_comp_rep)
            then raise Early_stop ;
            let n_common = n_l1 - n_l2 - n_ctxtcl2src in
            let sl1rep = c'_suffix n_common l1rep in
            r2 |> it_on_seqcl_reps (
              fun r2rep ->
                try
                  let pr2rep = c'_prefix n_common r2rep in
                  (match sl1rep,pr2rep with
                  | None,None -> ()
                  | Some (SeqId _),None -> ()
                  | None, Some (SeqId _) -> ()
                  | Some (SeqSeq u), Some (SeqSeq v) -> if u <> v then raise Early_stop
                  | _,_ -> failwith "???") ;
                  let sr2rep = c'_suffix (n_r2 - n_common) r2rep in
                  if (match sr2rep with None -> false
                                      | Some c -> c <> ctxtcl1tgt_r1_comp_rep)
                  then raise Early_stop ;
                  let new_l2_rep = match sl1rep with None -> l2_ctxtcl2tgt_comp_rep
                                                   | Some c -> c'_concat l2_ctxtcl2tgt_comp_rep c
                  and new_r1_rep = match sl1rep with None -> ctxtcl1src_r1_comp_rep
                                                   | Some c -> c'_concat c ctxtcl1src_r1_comp_rep
                  in
                  let new_l2 = get_seqcl new_l2_rep
                  and new_r1 = get_seqcl new_r1_rep
                  in
                  let new_w1 = get_ctxtcl @@ CtxtTriple (l2,ctxtcl2,new_r1)
                  and new_w2 = get_ctxtcl @@ CtxtTriple (new_l2,ctxtcl1,r1)
                  in
                  res := (new_w1,new_w2) :: !res
                with Early_stop -> ()
            )
          with
            Early_stop -> ()
      ) ;
      !res
    | _,_ -> failwith "Whiskers do not have the same shape"
  with
    Early_stop -> []

(** Compute all the pairs of classes that can be obtained by exchanging the two
   whiskers classes *)

and cpt_exch_btw_ctxtcl ctxtcl1 wdim1 ctxtcl2 wdim2 =
  cpt_cw_exch_btw_ctxtcl ctxtcl1 wdim1 ctxtcl2 wdim2
  @
  cpt_ccw_exch_btw_ctxtcl ctxtcl1 wdim1 ctxtcl2 wdim2

let convert_seq_to_pcell = function
  | SeqNull -> VoidCell
  | SeqSeq l -> Comp (List.map convert_ctxtcl_to_whisk l)
  | SeqId u -> Id (convert_seqcl_to_pcell u)

let convert_ctxt_to_whisk = function
  | CtxtGen g -> Gen g
  | CtxtTriple (l,iw,r) -> Whisk (convert_seqcl_to_pcell l, convert_ctxtcl_to_whisk iw,convert_seqcl_to_pcell r)

let pcell_eq c1 c2 =
  let seqcl1 = convert_pcell_to_seqcl c1 in
  let seqcl2 = convert_pcell_to_seqcl c2 in
  seqcl1 = seqcl2

let print_seqcls fnames =
  !seqcl_l |> List.iter (fun (SeqCl i,l) ->
      Printf.printf "seqclass %d with %d reps\n" i (List.length l) ;
      l |> List.iter (fun rep ->
          Printf.printf "One rep:\n";
          print_cell fnames @@ convert_seq_to_pcell rep;
          print_string "\n"
        )
    )

let print_ctxtcls fnames =
  !ctxtcl_l |> List.iter (fun (CtxtCl i, l) ->
      Printf.printf "ctxtclass %d with %d reps" i (List.length l) ;
      l |> List.iter (fun rep ->
          Printf.printf "One rep:\n";
          print_cell fnames @@ Comp [convert_ctxt_to_whisk rep];
          print_string "\n"
        )
    )

let comp_str_p p l r =
  let tl = c_tgt_p p l in
  let sr = c_src_p p r in
  if pcell_eq tl sr then
    comp_p p l r
  else
    raise @@ Invalid_argument "cells are not composable"

let ctxtcl_src ctxtcl =
  let Some reps = find_ctxtcl_reps ctxtcl in
  let rep = List.hd reps in


let seqcl_src seqcl =
  let Some reps = find_seqcl_reps seqcl in
  let rep = List.hd reps in
  match rep with
  | SeqNull -> raise @@ Invalid_argument "no source for SeqNull"
  | SeqId seqcl' -> seqcl'
  | SeqSeq ctxtcl_list ->
    let first_ctxtcl = List.hd ctxtcl_list in
    ctxtcl_src first_ctxtcl


let unit seqcl =
  get_seqcl (SeqId seqcl)

let l_comp 


let l_comp

(* SF : earlier attempt follows *)

(* TODO: factor out the bfs part *)
(* for simplicity, we check equality during the loop but we should do it before
   to save time *)


(* module Polword = struct
 *
 *   type uid = int
 *   type env = { fresh_id : unit -> uid ;
 *                mem_bfs_w : (s_ctxt -> ctxt list option) * (s_ctxt -> ctxt list -> unit) ;
 *                mem_norm_tot_w : (s_ctxt -> s_ctxt option) * (s_ctxt -> s_ctxt -> unit) ;
 *                mem_bfs_c : (s_cell -> cell list option) * (s_cell -> cell list -> unit)
 *              }
 *
 *   let rec w_bfs env w = failwith ""
 *
 *
 *
 *   and w_norm_part env w =
 *     match w with
 *     | Ctxt (l,iw,r) -> Ctxt (c_normal_tot env l, w_normal_part env iw, c_normal_tot env r)
 *     | Gen g -> Gen g.gen
 *
 *   and w_norm_tot env w =
 *     let sw = w_to_sw w in
 *     match fst env.mem_norm_tot_w sw with
 *     | Some u -> u
 *     | None ->
 *       match w with
 *       |
 *
 *   and c_norm_part env (c : cell) = match c with
 *     | Comp l -> Comp (List.map (w_normal_tot env) l)
 *     | Id id -> Id (c_normal_rep env id)
 *     | VoidCell -> VoidCell
 *
 *   and c_norm_tot env (c : cell) =
 *
 *   and
 *
 *
 *   let rec c_eq_wl_bfs goal l = match l with
 *     | [] -> false
 *     | (a :: q) -> failwith ""
 *
 *   let rec c_eq_wl wl1 wl2 =
 *     c_eq_wl_bfs wl1 [wl2]
 *
 *   let rec c_eq_aux1 c1 c2 = match (c1,c2) with
 *     | (Id c1', Id c2') -> c_eq_aux1 c1' c2'
 *     | (Comp l1, Comp l2) -> if List.length l1 <> List.length l2
 *       then false
 *       else c_eq_wl l1 l2
 *     | (VoidCell,VoidCell) -> true
 *     | _,_ -> false
 *
 *   let c_eq c1 c2 =
 *     let dim = c_dim c1 in
 *     if c_dim c2 <> dim then
 *       false
 *     else
 *       c_eq_aux1 c1 c2
 *
 * end *)
