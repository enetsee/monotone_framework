open Core_kernel
open Lang
open Monotone_framework_lib

include
  Monotone_framework.S
  with type t = Stmt.Labelled.t
   and type property =
              ( Arith_expr.Labelled.t
              , Arith_expr.Labelled.comparator_witness )
              Set_intf.Set.t
   and module Label := Stmt.Labelled.Label
   and module F := Stmt_flowgraph.Forward
