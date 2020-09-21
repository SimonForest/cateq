module Make :
  functor (OT : Set.OrderedType) ->
    sig
      type elmt = OT.t
      module S :
        sig
          type elt = OT.t
          type t = Set.Make(OT).t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val map : (elt -> elt) -> t -> t
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val min_elt_opt : t -> elt option
          val max_elt : t -> elt
          val max_elt_opt : t -> elt option
          val choose : t -> elt
          val choose_opt : t -> elt option
          val split : elt -> t -> t * bool * t
          val find : elt -> t -> elt
          val find_opt : elt -> t -> elt option
          val find_first : (elt -> bool) -> t -> elt
          val find_first_opt : (elt -> bool) -> t -> elt option
          val find_last : (elt -> bool) -> t -> elt
          val find_last_opt : (elt -> bool) -> t -> elt option
          val of_list : elt list -> t
        end
      module M :
        sig
          type key = OT.t
          type 'a t = 'a Map.Make(OT).t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val min_binding_opt : 'a t -> (key * 'a) option
          val max_binding : 'a t -> key * 'a
          val max_binding_opt : 'a t -> (key * 'a) option
          val choose : 'a t -> key * 'a
          val choose_opt : 'a t -> (key * 'a) option
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val find_opt : key -> 'a t -> 'a option
          val find_first : (key -> bool) -> 'a t -> key * 'a
          val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val find_last : (key -> bool) -> 'a t -> key * 'a
          val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      module IM :
        sig
          type key = int
          type +'a t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val min_binding_opt : 'a t -> (key * 'a) option
          val max_binding : 'a t -> key * 'a
          val max_binding_opt : 'a t -> (key * 'a) option
          val choose : 'a t -> key * 'a
          val choose_opt : 'a t -> (key * 'a) option
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val find_opt : key -> 'a t -> 'a option
          val find_first : (key -> bool) -> 'a t -> key * 'a
          val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val find_last : (key -> bool) -> 'a t -> key * 'a
          val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      type tfc = {
        gens : S.t;
        dim : elmt -> int;
        src : elmt -> S.t;
        tgt : elmt -> S.t;
      }
      type tf_scell = (S.t * S.t) list
      type tf_jcell = int * S.t
      type tf_mcell = S.t list
      val set_src : tfc -> S.t -> S.t
      val set_tgt : tfc -> S.t -> S.t
      val neg_border : tfc -> S.t -> S.t
      val pos_border : tfc -> S.t -> S.t
      val set_activation : tfc -> S.t -> S.t -> S.t
      val set_coactivation : tfc -> S.t -> S.t -> S.t
      exception Early_stop
      val set_test : (S.elt -> bool) -> S.t -> bool
      val set_map_union : (S.elt -> S.t) -> S.t -> S.t
      val list_replicate : int -> 'a -> 'a list
      val is_inter_empty : S.t -> S.t -> bool
      val is_movement : tfc -> S.t -> S.t -> S.t -> bool
      val satom_from_gen : tfc -> elmt -> (S.t * S.t) list
      val set_dim : tfc -> S.t -> int
      val is_set_forkfree : tfc -> S.t -> bool
      val check_srctgt_gen : tfc -> elmt -> bool
      val check_srctgt : tfc -> bool
      val check_nonempty_gen : tfc -> elmt -> bool
      val check_nonempty : tfc -> bool
      val check_relevance_gen : tfc -> elmt -> bool
      val check_relevance : tfc -> bool
      val check_graph_acyclicity : S.t -> (M.key -> M.key list) -> bool
      val cpt_tfc_ngbrs : tfc -> S.t M.t
      val check_tfc_acyclicity : tfc -> bool
      val check_two_sets_accessibility : S.t -> S.t -> (S.elt -> S.t) -> bool
      val check_strongsegment_gen : tfc -> elmt -> bool
      val check_strongsegment : tfc -> bool
      val check_strongtf : tfc -> bool
      val check_tfc : tfc -> bool
      type ('a, 'b) partial_cell =
          PGen of 'a
        | PId of 'b
        | PComp of int * 'b * 'b
      val cpt_tfc_tlr_ngbrs_on_set :
        tfc -> S.t -> (M.key -> S.t) * (M.key -> S.t)
      val find_graph_max : (S.elt -> S.t) -> S.elt -> S.elt
      val excision :
        tfc -> (S.t * S.t) list -> (S.elt, (S.t * S.t) list) partial_cell
      val cell_dec : tfc -> (S.t * S.t) list -> S.elt Cat.cell_form
      val set_to_jcell : tfc -> S.t -> int * S.t
      val cset_to_jcell : tfc -> S.t -> int * S.t
      val jcell_to_mcell : tfc -> IM.key * S.t -> S.t list
      val mcell_to_scell : tfc -> S.t list -> (S.t * S.t) list
      val jcell_to_scell : tfc -> IM.key * S.t -> (S.t * S.t) list
      val set_to_scell : tfc -> S.t -> (S.t * S.t) list
      val p_cell_to_cset :
        (S.elt -> int) ->
        (S.elt -> S.t) -> (S.elt -> S.t) -> S.elt Lib.Precat.p_cell -> S.t
      val p_whisk_to_cset :
        (S.elt -> int) ->
        (S.elt -> S.t) -> (S.elt -> S.t) -> S.elt Lib.Precat.p_whisk -> S.t
      val p_cell_to_topg_set : S.elt Lib.Precat.p_cell -> S.t
      val is_p_cell_top_generated : tfc -> S.elt Lib.Precat.p_cell -> bool
      val check_topgenerated :
        tfc ->
        (elmt -> S.elt Lib.Precat.p_cell) ->
        (elmt -> S.elt Lib.Precat.p_cell) -> bool
    end
module STP :
  sig
    type elmt = Lib.Precat.gen_st
    module S :
      sig
        type elt = Lib.Precat.gen_st
        type t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_list : elt list -> t
      end
    module M :
      sig
        type key = Lib.Precat.gen_st
        type +'a t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      end
    module IM :
      sig
        type key = int
        type +'a t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      end
    type tfc = {
      gens : S.t;
      dim : elmt -> int;
      src : elmt -> S.t;
      tgt : elmt -> S.t;
    }
    type tf_scell = (S.t * S.t) list
    type tf_jcell = int * S.t
    type tf_mcell = S.t list
    val set_src : tfc -> S.t -> S.t
    val set_tgt : tfc -> S.t -> S.t
    val neg_border : tfc -> S.t -> S.t
    val pos_border : tfc -> S.t -> S.t
    val set_activation : tfc -> S.t -> S.t -> S.t
    val set_coactivation : tfc -> S.t -> S.t -> S.t
    exception Early_stop
    val set_test : (S.elt -> bool) -> S.t -> bool
    val set_map_union : (S.elt -> S.t) -> S.t -> S.t
    val list_replicate : int -> 'a -> 'a list
    val is_inter_empty : S.t -> S.t -> bool
    val is_movement : tfc -> S.t -> S.t -> S.t -> bool
    val satom_from_gen : tfc -> elmt -> (S.t * S.t) list
    val set_dim : tfc -> S.t -> int
    val is_set_forkfree : tfc -> S.t -> bool
    val check_srctgt_gen : tfc -> elmt -> bool
    val check_srctgt : tfc -> bool
    val check_nonempty_gen : tfc -> elmt -> bool
    val check_nonempty : tfc -> bool
    val is_scell : tfc -> (S.t * S.t) list -> bool
    val check_relevance_gen : tfc -> elmt -> bool
    val check_relevance : tfc -> bool
    val check_graph_acyclicity : S.t -> (M.key -> M.key list) -> bool
    val cpt_tfc_ngbrs : tfc -> S.t M.t
    val check_tfc_acyclicity : tfc -> bool
    val check_two_sets_accessibility : S.t -> S.t -> (S.elt -> S.t) -> bool
    val check_strongsegment_gen : tfc -> elmt -> bool
    val check_strongsegment : tfc -> bool
    val check_strongtf : tfc -> bool
    val check_tfc : tfc -> bool
    type ('a, 'b) partial_cell =
        PGen of 'a
      | PId of 'b
      | PComp of int * 'b * 'b
    val cpt_tfc_tlr_ngbrs_on_set :
      tfc -> S.t -> (M.key -> S.t) * (M.key -> S.t)
    val find_graph_max : (S.elt -> S.t) -> S.elt -> S.elt
    val excision :
      tfc -> (S.t * S.t) list -> (S.elt, (S.t * S.t) list) partial_cell
    val cell_dec : tfc -> (S.t * S.t) list -> S.elt Cat.cell_form
    val set_to_jcell : tfc -> S.t -> int * S.t
    val cset_to_jcell : tfc -> S.t -> int * S.t
    val jcell_to_mcell : tfc -> IM.key * S.t -> S.t list
    val mcell_to_scell : tfc -> S.t list -> (S.t * S.t) list
    val jcell_to_scell : tfc -> IM.key * S.t -> (S.t * S.t) list
    val set_to_scell : tfc -> S.t -> (S.t * S.t) list
    val cset_to_scell : tfc -> S.t -> (S.t * S.t) list
    val p_cell_to_cset :
      (S.elt -> int) ->
      (S.elt -> S.t) -> (S.elt -> S.t) -> S.elt Lib.Precat.p_cell -> S.t
    val p_whisk_to_cset :
      (S.elt -> int) ->
      (S.elt -> S.t) -> (S.elt -> S.t) -> S.elt Lib.Precat.p_whisk -> S.t
    val p_cell_to_topg_set : S.elt Lib.Precat.p_cell -> S.t
    val is_p_cell_top_generated : tfc -> S.elt Lib.Precat.p_cell -> bool
    val check_topgenerated :
      tfc ->
      (elmt -> S.elt Lib.Precat.p_cell) ->
      (elmt -> S.elt Lib.Precat.p_cell) -> bool
  end
val cell_set_to_tfc : STP.S.elt Lib.Precat.p_cell list -> STP.tfc option
val cset_to_cf : STP.tfc -> STP.S.t -> STP.S.elt Cat.cell_form
val cf_to_cell : Lib.Precat.gen_st Cat.cell_form -> Lib.Precat.cell
val cset_to_cell : STP.tfc -> STP.S.t -> Lib.Precat.cell
