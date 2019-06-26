open Lang
open Monotone_framework

type t = Stmt.Labelled.t
type property = Arith_expr.Labelled.t StringMap.t option

val solve : t -> property entry_exit LabelMap.t
val example : t
