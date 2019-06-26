open Core_kernel
open Lib

module type Basic = sig
  type t

  module Label : Label.S

  val flow_of : t -> (Label.t * Label.t) list
  val initial_label_of : t -> Label.t
  val final_labels_of : t -> (Label.t, Label.comparator_witness) Set_intf.Set.t
  val associations : t -> (Label.t, t, Label.comparator_witness) Map_intf.Map.t
end

module type S = sig
  type t

  module Label : Label.S

  val flow_of : t -> (Label.t * Label.t) list

  val extremal_labels_of
    :  t
    -> (Label.t, Label.comparator_witness) Set_intf.Set.t

  val associations : t -> (Label.t, t, Label.comparator_witness) Map_intf.Map.t
end

module Make (X : Basic) : S with type t = X.t and module Label = X.Label =
struct
  type t = X.t

  module Label = X.Label
  module LabelSet = Set.Make_using_comparator (Label)

  let flow_of = X.flow_of
  let associations = X.associations
  let extremal_labels_of x = LabelSet.singleton @@ X.initial_label_of x
end

module Make_reverse (X : Basic) :
  S with type t = X.t and module Label = X.Label = struct
  type t = X.t

  module Label = X.Label
  module LabelSet = Set.Make_using_comparator (Label)

  let flow_of x = X.flow_of x |> List.map ~f:(fun (l1, l2) -> l2, l1)
  let associations = X.associations
  let extremal_labels_of x = X.final_labels_of x
end
