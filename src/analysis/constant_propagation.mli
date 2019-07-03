open Lib
open Lang
open Monotone_framework_lib

include
  Monotone_framework.S
  with type t = Stmt.Labelled.t
   and type property = Arith_expr.Labelled.t StringMap.t option
   and module Label := Stmt.Labelled.Label
   and module F := Stmt_flowgraph.Forward

val apply
  :  t Stmt.Labelled.LabelMap.t
     * property Monotone_framework.entry_exit Stmt.Labelled.LabelMap.t
  -> t option

val optimize : t -> t
