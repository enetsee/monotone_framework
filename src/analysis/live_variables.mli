open Core_kernel
open Lang
open Monotone_framework_lib

(* module Property : Set.S with module Elt := String *)

include
  Monotone_framework.S
  with type t = Stmt.Labelled.t
   and type property = (string, String.comparator_witness) Set.t
   and module Label := Stmt.Labelled.Label
   and module F := Stmt_flowgraph.Reverse
