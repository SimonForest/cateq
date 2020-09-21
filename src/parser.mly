%{
    open Catlang
%}

%token Tset Tcolon Tarrow Tpareno Tparenc Tbrao Tbrac Tcomma Teq Tload Tgen Tstar Teof

%token<int> Tcomp Tid

%token<string> Tvar Tfunident

%left Tcomp
%right Tid

%start<Catlang.op option> op
%%

var:
| s = Tvar { s }
;
vars:
| v = var ; Tcomma ; l = vars { v :: l }
| v = var { [v] }
;
setp:
| e = cellexpr ; Tcomma ; r = setp { e :: r }
| e = cellexpr { [ e ]}
set:
| Tbrao ; Tbrac { [] }
| Tbrao ; l = setp ; Tbrac { l }
;
cellexpr:
| s = var { CTVar s }
| l = cellexpr ; i = Tcomp ; r = cellexpr { CTComp (i,l,r) }
| n = Tid ; e = cellexpr { CTId (n,e) }
| Tpareno ; ct = cellexpr ; Tparenc { ct }
| s = set { CTPC s }
;
rvalue:
| ng = newgen { RVGen ng }
| e = cellexpr { RVCell e }
;
newgen:
| Tgen Tstar { NGZero }
| Tgen s = cellexpr ; Tarrow ; t = cellexpr { NGHigher (s,t) }
;
op:
| o = opp ; Teof { Some o }
| Teof { None }
opp:
| lv = vars ; Tset ; r = rvalue { OSet (lv,r) }
| l = cellexpr ; Teq ; r = cellexpr { OEq (l,r) }
| f = Tfunident ; arg = cellexpr {
                            match f with
                              "makkai" -> OMakkai arg
                            | "henry" -> OHenry arg
                            | _ -> failwith "undefined function"
                          }
