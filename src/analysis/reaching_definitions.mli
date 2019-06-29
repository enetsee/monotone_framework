open Core_kernel
open Lang
open Monotone_framework

type t = Stmt.Labelled.t

module Assignment : Lib.Label.S with type t = string * int option

type property = (Assignment.t, Assignment.comparator_witness) Set_intf.Set.t

val solve : t -> t LabelMap.t * property entry_exit LabelMap.t
val example_2_7 : Stmt.Labelled.t
