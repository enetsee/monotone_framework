open Lib
open Lang
open Monotone_framework_lib.Monotone_framework

type t = Stmt.Labelled.t
type property = Arith_expr.Labelled.t StringMap.t option

val solve
  :  t
  -> t Stmt.Labelled.LabelMap.t * property entry_exit Stmt.Labelled.LabelMap.t

val apply
  :  t Stmt.Labelled.LabelMap.t * property entry_exit Stmt.Labelled.LabelMap.t
  -> t option

val optimize : t -> t
val example : t

val show_solution
  :  property entry_exit Stmt.Labelled.LabelMap.t
  -> (int * ((string * string) list * (string * string) list)) list

val show_entry
  :  property entry_exit Stmt.Labelled.LabelMap.t
  -> (int * (string * string) list) list

val show_exit
  :  property entry_exit Stmt.Labelled.LabelMap.t
  -> (int * (string * string) list) list
