open Lib
open Precat
open Cat

(** Module for handling torsion-free complexes *)
module Make (OT : Set.OrderedType ) = struct

  type elmt = OT.t

  module S = Set.Make (OT)

  module M = Map.Make (OT)

  module IM = Map.Make (struct type t = int let compare = compare end)

  type tfc = { gens : S.t ;
               dim : elmt -> int ;
               src : elmt -> S.t ;
               tgt : elmt -> S.t }

  type tf_scell = (S.t * S.t) list

  type tf_jcell = int * S.t

  type tf_mcell = S.t list

  let set_src tfc set =
    S.fold (fun el s -> S.union s @@ tfc.src el) set S.empty

  let set_tgt tfc set =
    S.fold (fun el s -> S.union s @@ tfc.tgt el) set S.empty

  let neg_border tfc set =
    S.diff (set_src tfc set) @@ (set_tgt tfc set)

  let pos_border tfc set =
    S.diff (set_tgt tfc set) @@ (set_src tfc set)

  let set_activation tfc src set =
    S.diff (S.union src @@ set_tgt tfc set) (set_src tfc set)

  let set_coactivation tfc tgt set =
    S.diff (S.union tgt @@ set_src tfc set) (set_tgt tfc set)

  exception Early_stop

  let set_test f s =
    try
      s |> S.iter (fun x -> if not @@ f x then raise Early_stop) ; true
    with
      Early_stop -> false

  let set_map_union f s =
    let res = ref S.empty in
    s |> S.iter (fun x ->
        let os = f x in
        res := S.union os !res
      )
    ;
    !res

  let rec list_replicate n el =
    if n = 0 then []
    else
      el :: (list_replicate (n-1) el)


  let is_inter_empty u v =
    try
      S.iter
        (fun el -> if S.mem el v then raise Early_stop )
        u ; true
    with
    | Early_stop -> false

  let is_movement tfc s u v =
    let one =
      S.equal v @@ S.diff (S.union u (set_tgt tfc s)) (set_src tfc s)
    in
    let two =
      S.equal u @@ S.diff (S.union v (set_src tfc s)) (set_tgt tfc s)
    in
    one && two

  let satom_from_gen tfc g =
    let n = tfc.dim g in
    let rec aux n (s,t) =
      match n with
      | 0 -> [(s,t)]
      | _ -> (s,t) :: aux (n-1) (neg_border tfc s,pos_border tfc t)
    in
    aux n (S.singleton g,S.singleton g)


  let set_dim tfc set =
    S.fold (fun el m -> max m (tfc.dim el)) set (-1)

  (* test forkfreeness for sets of dim >= 1 !!! *)

  let is_set_forkfree tfc set =
    let res_src =
      try
        let _ = S.fold (fun el s ->
            let s_src = tfc.src el in
            if not @@ is_inter_empty s_src s
            then raise Early_stop
            else
              S.union s_src s
          ) set S.empty in true
      with
      | Early_stop -> false
    in
    let res_tgt =
      try
        let _ = S.fold (fun el s ->
            let s_tgt = tfc.tgt el in
            if not @@ is_inter_empty s_tgt s
            then raise Early_stop
            else
              S.union s_tgt s
          ) set S.empty in true
      with
      | Early_stop -> false
    in
    res_src && res_tgt


  let check_srctgt_gen tfc g =
    let n = tfc.dim g in
    if n <> 0 then
      let src = tfc.src g
      and tgt = tfc.tgt g
      in
      S.fold (fun el b -> b && tfc.dim el = n-1) src true
      && S.fold (fun el b -> b && tfc.dim el = n-1) tgt true
      && S.subset src tfc.gens
      && S.subset tgt tfc.gens
    else if n < 0 then
      false
    else
      true

  let check_srctgt tfc =
    set_test (check_srctgt_gen tfc) tfc.gens

  let check_nonempty_gen tfc g =
    if tfc.dim g <> 0 then
      not @@ (S.is_empty @@ tfc.src g) || (S.is_empty @@ tfc.tgt g)
    else
      true

  let check_nonempty tfc =
    set_test (check_nonempty_gen tfc) tfc.gens

  let is_scell tfc c =
    let rec aux = function
      | [] -> false
      | [ (s,t) ] -> S.cardinal s = 1 && S.cardinal t = 1
      | (s,t) :: ((s',t') :: q as q') ->
        if is_set_forkfree tfc s
        && is_set_forkfree tfc t
        && is_movement tfc s s' t'
        && is_movement tfc t s' t'
        then
          aux q'
        else
          raise Early_stop
    in
    try
      aux c
    with
    | Early_stop -> false

  let check_relevance_gen tfc g =
    let at = satom_from_gen tfc g in
    is_scell tfc at
    (* let rec aux = function
     *   | [ (s,t) ] -> S.cardinal s = 1 && S.cardinal t = 1
     *   | (s,t) :: ((s',t') :: q as q') ->
     *     if is_set_forkfree tfc s
     *     && is_set_forkfree tfc t
     *     && is_movement tfc s s' t'
     *     && is_movement tfc t s' t'
     *     then
     *       aux q'
     *     else
     *       raise Early_stop
     *   | _ -> raise @@ Invalid_argument ""
     * in
     * try
     *   aux at
     * with
     * | Early_stop -> false *)

  let check_relevance tfc =
    set_test (check_relevance_gen tfc) tfc.gens

  let check_graph_acyclicity nodes fngbrs =
    let states = ref M.empty in
    let rec aux node =
      let state_opt = M.find_opt node !states
      in
      match state_opt with
      | Some 2 -> ()
      | Some 1 -> raise Early_stop
      | _ ->
        states := M.add node 1 !states;
        let ngbrs = fngbrs node in
        ngbrs |> List.iter (fun n ->
            aux n
          );
        states := M.add node 2 !states
    in
    try (S.iter aux nodes) ; true with
    | Early_stop -> false

  let cpt_tfc_ngbrs tfc =
    let ngbrs = ref M.empty in
    tfc.gens |> S.iter (fun x -> ngbrs := M.add x S.empty !ngbrs );
    tfc.gens |> S.iter (fun node ->
        match tfc.dim node with
          0 -> ()
        | n ->
          let src = tfc.src node in
          let tgt = tfc.tgt node in
          src |> S.iter (fun b ->
              ngbrs := M.add b (S.union tgt @@ M.find b !ngbrs) !ngbrs
            )) ;
    !ngbrs
  (* neighbors for the \curlyabove relation *)
  let cpt_jumpabove_ngbrs tfc =
    let ngbrs = ref M.empty in
    tfc.gens |> S.iter (fun x -> ngbrs := M.add x S.empty !ngbrs );
    tfc.gens |> S.iter (fun node ->
        let at = satom_from_gen tfc node in
        let rec aux = function
          | [] -> ()
          | (src,tgt) :: q ->
            src |> S.iter (fun b ->
                ngbrs := M.add b (S.union tgt @@ M.find b !ngbrs) !ngbrs
              )
          ; aux q
        in
        aux @@ List.tl at
      )
    ;
    !ngbrs


  let check_tfc_acyclicity tfc =
    let ngbrs = cpt_tfc_ngbrs tfc in
    let fngbrs n =
      S.elements @@ M.find n ngbrs
    in
    check_graph_acyclicity tfc.gens fngbrs

  let check_two_sets_accessibility set1 set2 fngbrs =
    let already = ref S.empty in
    let rec aux node =
      match S.mem node !already with
      | true -> ()
      | false ->
        already := S.add node !already ;
        if S.mem node set2 then
          raise Early_stop ;
        fngbrs node |> S.iter aux
    in
    try (set1 |> S.iter aux) ; false with
    | Early_stop -> true

  let check_strongsegment_gen tfc gen =
    let ngbrs = cpt_tfc_ngbrs tfc in
    let fngbrs n = M.find n ngbrs in
    let at = satom_from_gen tfc gen in
    let rec aux = function
      | [] -> true
      | (a,b) :: q -> not (check_two_sets_accessibility b a fngbrs) && aux q
    in
    aux @@ List.tl at

  let check_strongsegment tfc =
    try
      tfc.gens |> S.iter (fun g ->
          if not @@ check_strongsegment_gen tfc g then
            raise Early_stop
        )
    ; true
    with
      Early_stop -> false

  let check_strongtf tfc =
    let ngbrs = cpt_tfc_ngbrs tfc in
    let fngbrs n = M.find n ngbrs in
    try
      tfc.gens |> S.iter (fun x ->
          (* reduced-reversed atom *)
          let rratx = List.rev @@ List.tl @@ satom_from_gen tfc x in
          tfc.gens |> S.iter (fun y ->
              let rraty = List.rev @@ List.tl @@ satom_from_gen tfc y in
              let rec aux l1 l2 = match l1,l2 with
                | (a1m,a1p)::((a2m,a2p)::qa as inda),(b1m,b1p)::((b2m,b2p)::qb as indb) ->
                  (
                    (not @@ is_inter_empty a2p b2m)
                    || (not @@ check_two_sets_accessibility a1p b1m fngbrs)
                    || (not @@ check_two_sets_accessibility b1p a1m fngbrs)
                  )
                  &&
                  aux inda indb
                | _ -> true
              in
              if not @@ aux rratx rraty then
                raise Early_stop
            )
        )
    ; true
    with
      Early_stop -> false

  let check_tfc tfc =
    check_srctgt tfc
    && check_nonempty tfc
    && check_relevance tfc
    && check_tfc_acyclicity tfc
    && check_strongsegment tfc
    && check_strongtf tfc

  type ('a,'b) partial_cell =
    | PGen of 'a
    | PId of 'b
    | PComp of int * 'b * 'b

  (* tlr : triangle (relation) left / right *)
  let cpt_tfc_tlr_ngbrs_on_set tfc set =
    let src_to_g,tgt_to_g = (ref M.empty,ref M.empty) in
    set |> S.iter (fun g ->
        tfc.src g |> S.iter (fun srcg ->
            let old = try M.find srcg !src_to_g with Not_found -> S.empty in
            src_to_g := M.add srcg (S.union old (S.singleton g)) !src_to_g
          )
        ;
        tfc.tgt g |> S.iter (fun tgtg ->
            let old = try M.find tgtg !tgt_to_g with Not_found -> S.empty in
            tgt_to_g := M.add tgtg (S.union old (S.singleton g)) !tgt_to_g
          )
      ) ;
    let be,af = ref M.empty, ref M.empty in
    set |> S.iter (fun g ->
        tfc.src g |> S.iter (fun srcg ->
            let old = try M.find g !be with Not_found -> S.empty in
            let value = try M.find srcg !tgt_to_g with Not_found -> S.empty in
            be := M.add g (S.union old value) !be
          )
        ;
        tfc.tgt g |> S.iter (fun tgtg ->
            let old = try M.find g !af with Not_found -> S.empty in
            let value = try M.find tgtg !src_to_g with Not_found -> S.empty in
            af := M.add g (S.union old value) !af
          )

      ) ;
    let bef = fun g -> try M.find g !be with Not_found -> S.empty in
    let aff = fun g -> try M.find g !af with Not_found -> S.empty in
    (bef,aff)

  let rec find_graph_max fnext el =
    let nexts = fnext el in
    if S.is_empty nexts then el
    else
      find_graph_max fnext (S.choose nexts)

  let excision tfc cell =
    match cell with
    | [] -> raise @@ Invalid_argument "empty cell"
    | (tops,topt) :: tailc ->
      match S.is_empty tops with
      | true -> PId tailc
      | false ->
        let randel = S.choose tops in
        let rec non_eq_dec above below set =
          match below with
          | [] -> (above,set,[])
          | (bs,bt) :: q ->
            if S.equal bs set then
              non_eq_dec ((bs,bt)::above) q (if q <> [] then neg_border tfc set else S.empty)
            else
              (above,set,below)
        in
        let (above,gensrc,below) = non_eq_dec [] cell (S.singleton randel) in
        match below with
        | [] -> PGen randel
        | (xs,xt) :: lower ->
          (* TODO check that it is correct not to remove gensrc from the intersection *)
          let w = S.choose (S.inter xs xt) in
          let bef,aff = cpt_tfc_tlr_ngbrs_on_set tfc xs in
          let (ys,yt),lowlower = match lower with
            | (ys,yt) :: lowlower -> ((ys,yt),lowlower)
            | [] -> failwith "should not happen"
          in
          let wmax = find_graph_max aff w in
          if not @@ S.mem wmax gensrc then
            let ymid = set_coactivation tfc yt (S.singleton wmax) in
            let lcell = (List.rev above) @ (S.remove wmax xs,S.remove wmax xt) :: (ys,ymid) :: lowlower
            in
            let rcell = (list_replicate (List.length above) (S.empty,S.empty)) @ (S.singleton wmax,S.singleton wmax) :: (ymid,yt) :: lowlower
            in
            PComp (List.length lowlower,lcell,rcell)
          else
            let wmin = find_graph_max bef w in
            let ymid = set_activation tfc ys (S.singleton wmin) in
            let lcell = (list_replicate (List.length above) (S.empty,S.empty)) @ (S.singleton wmin,S.singleton wmin) :: (ys,ymid) :: lowlower in
            let rcell = (List.rev above) @ (S.remove wmin xs,S.remove wmin xt) :: (ymid,yt) :: lowlower in
            PComp (List.length lowlower,lcell,rcell)

  let rec cell_dec tfc cell =
    match excision tfc cell with
    | PId u -> CFId (cell_dec tfc u)
    | PComp (n,l,r) -> CFComp (n,cell_dec tfc l,cell_dec tfc r)
    | PGen g -> CFGen g

  let set_to_jcell tfc s =
    let maxn = ref 0 in
    let rec aux already nodes = match nodes with
      | [] -> already
      | a :: q ->
        if S.mem a already then aux already q
        else
          let n = tfc.dim a in
          maxn := max n !maxn;
          if n > 0 then
            aux (S.add a already) (S.elements (S.union (tfc.src a) (tfc.tgt a)) @ nodes )
          else
            aux (S.add a already) nodes
    in
    let cell = aux S.empty (S.elements s) in
    (!maxn,cell)

  let cset_to_jcell tfc s =
    let n = set_dim tfc s in
    (n,s)

  let jcell_to_mcell tfc jc =
    let (n,s) = jc in
    let gen_by_dims = ref IM.empty in
    s |> S.iter (fun g ->
        let n = tfc.dim g in
        let old = try IM.find n !gen_by_dims with Not_found -> S.empty in
        gen_by_dims := IM.add n (S.add g old) !gen_by_dims
      )
    ;
    let rec aux toremove k =
      let currset = try IM.find k !gen_by_dims with Not_found -> S.empty in
      let s = S.diff currset toremove in
      match k with
      | -1 -> []
      | 0 -> [ s ]
      | _ -> let ntoremove = S.union (set_src tfc currset) (set_tgt tfc currset) in
        s :: (aux ntoremove (k-1))
    in
    aux S.empty n

  let rec mcell_to_scell tfc mc =
    match mc with
    | [] -> []
    | [ s ] -> [ s , s ]
    | s :: t :: q ->
      let t' = S.union t (neg_border tfc s) in
      let src_scell = mcell_to_scell tfc (t' :: q) in
      match src_scell with
      | (ys,_) :: lower ->
        let yt = set_activation tfc ys s in
        (s,s) :: (ys,yt) :: lower
      | _ -> failwith "not supposed to happen"

  let jcell_to_scell tfc s =
    s |> jcell_to_mcell tfc |> mcell_to_scell tfc
  let set_to_scell tfc s =
    s |> set_to_jcell tfc |> jcell_to_scell tfc

  let cset_to_scell tfc s =
    s |> cset_to_jcell tfc |> jcell_to_scell tfc

  let rec p_cell_to_cset fdim fsrc ftgt pc = match pc with
    | Id u -> p_cell_to_cset fdim fsrc ftgt u
    | Comp l -> List.fold_left S.union S.empty
      @@ List.map (p_whisk_to_cset fdim fsrc ftgt) l
    | VoidCell -> S.empty

  and p_whisk_to_cset fdim fsrc ftgt pw = match pw with
    | Whisk (l,iw,r) -> S.union (p_cell_to_cset fdim fsrc ftgt l)
      @@ S.union (p_whisk_to_cset fdim fsrc ftgt iw) (p_cell_to_cset fdim fsrc ftgt r)
    | Gen g -> if fdim g = 0 then
        S.singleton g
      else
        let rec aux already = function
          | [] -> already
          | a :: q ->
            if S.mem a already then
              aux already q
            else if fdim a <= 0 then
              aux (S.add a already) q
            else
              let children = (S.elements @@ fsrc a) @ (S.elements @@ ftgt a) in
              aux (S.add a already) (children @ q)
        in
        aux S.empty [g]

  let p_cell_to_topg_set pc = match pc with
    | Comp l -> List.fold_left (fun s x -> S.add x s) S.empty @@ List.map w_get_gen l
    | _ -> S.empty

  let is_p_cell_top_generated tfc c =
    let clc = p_cell_to_cset tfc.dim tfc.src tfc.tgt c in
    let tgc = p_cell_to_topg_set c in
    S.equal clc (snd (set_to_jcell tfc tgc))

  let check_topgenerated tfc csrc_f ctgt_f =
    let aux g = match tfc.dim g with
      | 0 -> ()
      | _ -> if not @@ (is_p_cell_top_generated tfc (csrc_f g)
             && is_p_cell_top_generated tfc (ctgt_f g))
        then
          raise Early_stop
    in
    try tfc.gens |> S.iter aux ; true with
    | Early_stop -> false
end

module STP = Make (struct type t = gen_st let compare = compare end)

let cell_set_to_tfc cs =
  let dimm = ref STP.M.empty in
  let fdim x =
    try STP.M.find x !dimm with
    | Not_found -> (let d = g_dim x in
                    dimm := STP.M.add x d !dimm ; d)
  in
  let srcm = ref STP.M.empty in
  let fsrc x =
    try STP.M.find x !srcm with
    | Not_found -> (let src = STP.p_cell_to_topg_set x.src in
                    srcm := STP.M.add x src !srcm ;
                    src
                   )
  in
  let tgtm = ref STP.M.empty in
  let ftgt x =
    try STP.M.find x !tgtm with
    | Not_found -> (let tgt = STP.p_cell_to_topg_set x.tgt in
                    tgtm := STP.M.add x tgt !tgtm ;
                    tgt)
  in
  let gens = List.fold_left STP.S.union STP.S.empty
    @@ List.map (STP.p_cell_to_cset fdim fsrc ftgt) cs
  in
  let tfc = STP.{ gens = gens ; dim = fdim ; src = fsrc ; tgt = ftgt }
  in
  (* TODO debug and replace with the code below *)
  if not @@ STP.check_topgenerated tfc g_src g_tgt then
    None
  else
  if not @@ STP.check_tfc tfc
  then
    None
  else Some tfc
  (* if STP.check_topgenerated tfc g_src g_tgt && STP.check_tfc tfc
   * then Some tfc
   * else None *)

let cset_to_cf tfc cs =
  let dim = STP.set_dim tfc cs in
  let js = (dim,cs) in
  let sc = STP.jcell_to_scell tfc js in
  STP.cell_dec tfc sc

let rec cf_to_cell = function
  | CFGen g -> gen_to_cell g
  | CFComp (p,l,r) -> comp_p p (cf_to_cell l) (cf_to_cell r)
  | CFId (u) -> id_cell (cf_to_cell u)

let cset_to_cell tfc cs =
  let sc = STP.cset_to_scell tfc cs in
  if not (STP.is_scell tfc sc) then
    raise @@ Invalid_argument "this complex has no unique maximal cell" ;
  sc |> STP.cell_dec tfc |> cf_to_cell
