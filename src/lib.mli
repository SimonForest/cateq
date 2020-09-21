module Precat :
  sig
    type gen = int

    (* module OrdGen : sig
     *   type t = gen
     *   val compare : t -> t -> int
     * end *)

    type 'a p_cell = Comp of 'a p_whisk list | Id of 'a p_cell | VoidCell
    and 'a p_whisk = Gen of 'a | Whisk of 'a p_cell * 'a p_whisk * 'a p_cell

    type wdim = { n_gen : int ; n_topw : int }
    type gen_st = { gen : gen ; src : gen_st p_cell ; tgt : gen_st p_cell }

    type cell = gen_st p_cell
    type whisk = gen_st p_whisk

    type s_cell = gen p_cell
    type s_whisk = gen p_whisk

    (* val c_to_sc : cell -> s_cell
     * val w_to_sw : whisk -> s_whisk *)

    val g_src : gen_st -> cell
    val g_tgt : gen_st -> cell
    (* val g_name : gen -> string *)
    val c_dim : cell -> int
    val w_gen_dim : whisk -> int
    val w_whisk_dim : whisk -> int
    val g_dim : gen_st -> int
    val c_concat : 'a p_cell -> 'a p_cell -> 'a p_cell
    val comp : cell -> cell -> cell
    val w_l_comp : cell -> whisk -> whisk
    val w_r_comp : whisk -> cell -> whisk
    val comp3 : cell -> cell -> cell -> cell
    val w_src : whisk -> cell
    val w_tgt : whisk -> cell
    val c_src : cell -> cell
    val c_tgt : cell -> cell
    val c_src_p : int -> cell -> cell
    val c_tgt_p : int -> cell -> cell
    val wdim_wdec : wdim -> wdim
    val w_src_p : int -> whisk -> cell
    val w_src_relp : int -> gen_st p_whisk -> gen_st p_cell
    val w_tgt_p : int -> whisk -> cell
    val w_tgt_relp : int -> gen_st p_whisk -> gen_st p_cell
    val c_test : cell -> unit
    val w_test : whisk -> unit
    val w_get_gen : 'a p_whisk -> 'a
    val size_cell : 'a p_cell -> int
    val gen_to_whisk : gen_st -> whisk
    val void_cell : cell
    val create_gen : gen -> cell -> cell -> gen_st
    val compose_cells : cell -> cell -> cell
    val id_cell : cell -> cell
    val id_cell_p : int -> gen_st p_cell -> gen_st p_cell
    val gen_to_cell : gen_st -> cell
    val create_gen_cell :
      gen -> cell -> cell -> gen_st * cell
    val str_from_gen : (int -> string) option -> int -> string
    val print_cell : (gen -> string) option -> cell -> unit
    val print_whisk : (gen -> string) option -> whisk list -> unit
    (* val string_from_cell : cell -> string *)
    (* type reduction_rule = string * (cell * cell)
     * val red_name : 'a * 'b -> 'a
     * val red_source : 'a * ('b * 'c) -> 'b
     * val red_target : 'a * ('b * 'c) -> 'c
     * val w_extract_gen : whisk -> gen
     * val whisk_printer : Format.formatter -> (gen -> string) option -> whisk -> unit
     * val cell_printer : Format.formatter -> (gen -> string) option -> cell -> unit
     * val is_prefix : 'a list -> 'a list -> bool
     * val find_matching_superpositions : 'a list -> 'a list -> int list
     * val compute_superposition_zone1 : int -> int -> int -> int * int
     * val compute_superposition_zone2 : int -> int -> int -> int * int
     * val compute_first_common : int -> int -> int -> int * int
     * val is_positive_index : 'a -> 'b -> 'a -> bool
     * val get_sublist_from_range : 'a list -> int -> int -> 'a list
     * type ctxt = Hole | CtxtWhisk of (cell option * ctxt * cell option)
     * exception Debug of (cell * ctxt * cell * ctxt)
     * val apply_ctxt_to_cell : ctxt -> cell -> cell
     * val apply_ctxt_to_whisk : ctxt -> whisk -> whisk
     * val cell_to_list : cell -> whisk list
     * val find_unifying_contexts : whisk -> whisk -> (ctxt * ctxt) option
     * val find_cell_superpositions : cell -> cell -> bool -> (ctxt * ctxt) list
     * val remove_none : 'a option list -> 'a list
     * val compute_up_excp :
     *   gen list -> whisk -> (gen * (bool * (ctxt * ctxt))) list
     * val compute_down_excp :
     *   gen list -> whisk -> (gen * (bool * (ctxt * ctxt))) list
     * val get_subcell_from_range : cell -> int -> int -> cell
     * val are_exchangeable_counterclockwise : whisk -> whisk -> bool
     * val are_exchangeable_clockwise : whisk -> whisk -> bool
     * val are_exchangeable : whisk -> whisk -> bool
     * val compute_exch_red : whisk -> whisk -> bool -> gen
     * val compute_exch_red_inner_to_cell : cell -> gen list
     * val compute_exch_red_with_cell : gen list -> cell -> gen list
     * val compute_normal_cps :
     *   ('a * (cell * 'b)) list ->
     *   (('a * (cell * 'b)) * ('a * (cell * 'b)) * (ctxt * ctxt)) list
     * val compute_exch_cps :
     *   gen list ->
     *   reduction_rule list -> (reduction_rule * gen * (ctxt * ctxt)) list
     * val compute_cps :
     *   gen list ->
     *   reduction_rule list ->
     *   (reduction_rule * reduction_rule * (ctxt * ctxt)) list *)
  end
