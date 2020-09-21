open Lib
open Precat
open Cat
open Polword
open Parity
open Makkai

(* type token =
 *     Tset
 *   | Tcolon
 *   | Tarrow
 *   | Tpareno
 *   | Tparenc
 *   | Teq
 *   | Tload
 *   | Tgen
 *   | Tcomp of int
 *   | Tid of int
 *   | Tvar of string
 *   | Teof *)

type varid = string

type celltree =
  | CTComp of (int * celltree * celltree)
  | CTId of (int * celltree)
  | CTVar of varid
  | CTPC of celltree list

type newgen =
  | NGZero
  | NGHigher of (celltree * celltree)

type rvalue =
  | RVGen of newgen
  | RVCell of celltree

type op =
  | OSet of (varid list * rvalue)
  | OEq of (celltree * celltree)
  | OMakkai of celltree
  | OHenry of celltree

module IM = Map.Make (struct type t = int let compare = compare end)

module SM = Map.Make (struct type t = varid let compare = compare end)

type exenv =
  {
    fresh : unit -> int ;
    vars : Precat.cell SM.t ;
    names : string IM.t
  }

let new_exenv () =
  let fresh =
    let r = ref (-1) in
    fun () -> (incr r ; !r)
  in
  {
    fresh = fresh ;
    vars = SM.empty ;
    names = IM.empty
  }

let find_var vid env : Precat.cell =
  try SM.find vid env.vars with
  | Not_found -> raise @@ Invalid_argument (Printf.sprintf "%s not defined" vid)
let rec eval_ct env : _ -> Precat.cell = function
  (* | CTVar vid -> SM.find vid env.vars *)
  | CTVar vid -> find_var vid env
  | CTId (n,ct) ->
    let ct' = eval_ct env ct in
    id_cell_p n ct'
  | CTComp (n,l,r) ->
    let l' = eval_ct env l
    and r' = eval_ct env r
    in
    comp_str_p n l' r'
  | CTPC l ->
    let l' = List.map (eval_ct env) l in
    match cell_set_to_tfc l' with
    (* TODO change exception type *)
    | None -> raise @@ Invalid_argument "not a torsion-free complex"
    | Some tfc -> cset_to_cell tfc tfc.STP.gens

let exec_op env op =
  match op with
  | OSet (vl,rv) ->
    let resf =
      match rv with
      | RVGen ng ->
        (
          match ng with
          | NGZero ->
            fun () ->
              let id = env.fresh () in
              let c = gen_to_cell { gen = id ; src = VoidCell ; tgt = VoidCell } in
              (c, Some id)
          | NGHigher (s,t) ->
            let sc,tc = eval_ct env s, eval_ct env t in
            let sc_dim, tc_dim = c_dim sc, c_dim tc in
            if sc_dim <> tc_dim then
              raise @@ Invalid_argument "source and target of gen have not same dimension" ;
            if sc_dim >= 1 && not ((pcell_eq (c_src sc) (c_src tc)) && (pcell_eq (c_tgt sc) (c_tgt tc))) then
              raise @@ Invalid_argument "source and target of gen are not parallel" ;
            (* TODO check dimension equality here *)
            fun () ->
              let id = env.fresh () in
              let c = gen_to_cell { gen = id ; src = sc ; tgt = tc } in
              (c, Some id)
        )
      | RVCell ct ->
        let c = eval_ct env ct in
        fun () -> (c,None)
    in
    let nenv = vl |> List.fold_left (fun env v ->
        let (c,idopt) = resf () in
        let env = match idopt with
          | None -> env
          | Some id -> { env with names = IM.add id v env.names }
        in
        let env = { env with vars = SM.add v c env.vars }
        in
        env
      ) env in
    (nenv,None)
    (* let env = match idopt with
     *   | None -> env
     *   | Some id -> { env with names = IM.add id varname env.names }
     * in
     * let env = { env with vars = SM.add varname c env.vars } in
     * (env,None) *)
  | OEq (l,r) ->
    let cl,cr = eval_ct env l,eval_ct env r in
    let res =
      (* let fnames = fun id -> IM.find id env.names in
       * print_cell (Some fnames) cl ;
       * print_cell (Some fnames) cr ; *)
      if convert_pcell_to_seqcl cl = convert_pcell_to_seqcl cr then
        "true"
      else
        "false"
    in
    (env,Some res)
  | OMakkai arg ->
    begin
      let c = eval_ct env arg in
      let res = makkai c in
      let fnames = fun id -> IM.find id env.names in
      let buf = Buffer.create 10 in
      let fmt = Format.formatter_of_buffer buf in
      mod_printer fmt (Some fnames) res;
      Format.pp_print_flush fmt ();
      (env, Some (Buffer.contents buf))
    end
  | OHenry arg ->
    begin
      let c = eval_ct env arg in
      let res = delta c in
      let fnames = fun id -> IM.find id env.names in
      let buf = Buffer.create 10 in
      let fmt = Format.formatter_of_buffer buf in
      mod_printer fmt (Some fnames) res;
      Format.pp_print_flush fmt ();
      (env, Some (Buffer.contents buf))
    end


