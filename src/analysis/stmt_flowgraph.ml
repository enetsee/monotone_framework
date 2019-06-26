open Core_kernel
open Lang

module Stmt_flow :
  Flowgraph.Basic
  with type t = Stmt.Labelled.t
   and module Label = Stmt.Labelled.Label = struct
  type t = Stmt.Labelled.t

  module Label = Stmt.Labelled.Label
  module LabelSet = Set.Make_using_comparator (Label)

  let initial_label_of x = Common.initial x
  let final_labels_of x = Common.finals x
  let flow_of x = Common.flow x
  let associations x = Common.associate x |> Associations.stmt_map
end

module Forward = Flowgraph.Make (Stmt_flow)
module Reverse = Flowgraph.Make_reverse (Stmt_flow)
