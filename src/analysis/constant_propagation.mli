open Lang
open Monotone_framework

type t = Stmt.Labelled.t
type property = Arith_expr.Labelled.t StringMap.t option

val solve : t -> property entry_exit LabelMap.t
val example : t

val show_solution
  :  property entry_exit LabelMap.t
  -> (int * ((string * string) list * (string * string) list)) list

val show_entry
  :  property entry_exit LabelMap.t
  -> (int * (string * string) list) list

val show_exit
  :  property entry_exit LabelMap.t
  -> (int * (string * string) list) list
