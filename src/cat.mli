type 'a cell_form =
  | CFGen of 'a
  | CFComp of int * 'a cell_form * 'a cell_form
  | CFId of 'a cell_form

val comp_p : int -> Lib.Precat.cell -> Lib.Precat.cell -> Lib.Precat.cell
