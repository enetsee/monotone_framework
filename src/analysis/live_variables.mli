open Core_kernel
open Lang
open Monotone_framework_lib

include
  Monotone_framework.S
  with type t = Stmt.Labelled.t
   and type property = (string, String.comparator_witness) Set.t
   and module Label := Stmt.Labelled.Label
   and module F := Stmt_flowgraph.Reverse

val apply
  :  t Stmt.Labelled.LabelMap.t
     * property Monotone_framework.entry_exit Stmt.Labelled.LabelMap.t
  -> t option

val optimize : t -> t
