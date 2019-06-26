open Core_kernel
open Lang
open Monotone_framework

type t = Stmt.Labelled.t

type property =
  ( Arith_expr.Labelled.t
  , Arith_expr.Labelled.comparator_witness )
  Set_intf.Set.t

module Label = Stmt.Labelled.Label
module LabelMap : Map.S with module Key := Label

val solve : t -> property entry_exit LabelMap.t
val example_2_5 : Stmt.Labelled.t

(* val solve : Stmt.Labelled.t -> (Stmt.Labelled.Label.t,
   (Arith_expr.Labelled.t , Arith_expr.Labelled.comparator_witness)
   Set_intf.Set.t Monotone_framework.entry_exit,
   Stmt.Labelled.Label.comparator_witness) Map_intf.Map.t

   val example_2_5 : Stmt.Labelled.t *)
