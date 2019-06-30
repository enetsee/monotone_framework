open Lang
open Monotone_framework_lib

module Forward :
  Flowgraph.S
  with type t = Stmt.Labelled.t
   and module Label = Stmt.Labelled.Label

module Reverse :
  Flowgraph.S
  with type t = Stmt.Labelled.t
   and module Label = Stmt.Labelled.Label
