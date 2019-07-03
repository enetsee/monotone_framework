open Core_kernel
open Lang
open Monotone_framework_lib
module Assignment : Lib.Label.S with type t = string * int option

include
  Monotone_framework.S
  with type t = Stmt.Labelled.t
   and type property =
              (Assignment.t, Assignment.comparator_witness) Set_intf.Set.t
   and module Label := Stmt.Labelled.Label
   and module F := Stmt_flowgraph.Forward
