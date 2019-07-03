open Core_kernel

type 'p entry_exit =
  { entry : 'p
  ; exit : 'p
  }

let entry { entry; _ } = entry
let exit { exit; _ } = exit

module type S = sig
  type t
  type property

  module Label : Lib.Label.S
  module F : Flowgraph.S with type t := t and module Label := Label
  module L : Lattice.S with type t := t and type property := property

  module TF :
    Transfer_function.S with type t := t and type property := property

  val solve
    :  t
    -> (Label.t, t, Label.comparator_witness) Map_intf.Map.t
       * ( Label.t
         , property entry_exit
         , Label.comparator_witness )
         Map_intf.Map.t
end

module Make
    (F : Flowgraph.S)
    (L : Lattice.S with type t := F.t)
    (TF : Transfer_function.S
          with type t := F.t
           and type property := L.property) :
  S
  with type t := F.t
   and module Label := F.Label
   and module F := F
   and type property := L.property
   and module L := L
   and module TF := TF = struct
  module LabelMap = Map.Make_using_comparator (F.Label)
  module LabelSet = Set.Make_using_comparator (F.Label)

  (** Add elements to the worklist (see Step 2, page 75, Nielsen et al.) *)
  let rec enqueue label accu = function
    | [] -> accu
    | (l, l') :: rest when l = label -> enqueue label ((l, l') :: accu) rest
    | _ :: rest -> enqueue label accu rest
  ;;

  let update_analysis analysis label prop =
    LabelMap.update analysis label ~f:(fun prop_opt ->
        L.lub (Option.value_exn prop_opt) prop)
  ;;

  let rec solve_helper all_props flow assocs analysis = function
    | [] -> analysis
    | (l1, l2) :: rest ->
      let t = LabelMap.find_exn assocs l1 in
      let props1 = TF.apply all_props t @@ LabelMap.find_exn analysis l1
      and props2 = LabelMap.find_exn analysis l2 in
      if not (L.leq props1 props2)
      then (
        let analysis' = update_analysis analysis l2 props1
        and w = enqueue l2 rest flow in
        solve_helper all_props flow assocs analysis' w)
      else solve_helper all_props flow assocs analysis rest
  ;;

  let result_of_analysis all_props flow assocs analysis =
    List.map ~f:fst flow
    |> List.map ~f:(fun lbl ->
           let t = LabelMap.find_exn assocs lbl
           and entry = LabelMap.find_exn analysis lbl in
           let exit = TF.apply all_props t entry in
           lbl, { entry; exit })
    |> LabelMap.of_alist_exn
  ;;

  let initialize initials extremal_value least_value flow =
    let flow_source = List.map ~f:fst flow
    and flow_sink = List.map ~f:snd flow
    and extremal_labels = Set.to_list initials in
    List.fold_left ~init:LabelMap.empty ~f:(fun accu label ->
        let data =
          if LabelSet.mem initials label then extremal_value else least_value
        in
        match LabelMap.add accu ~key:label ~data with
        | `Duplicate -> accu
        | `Ok accu' -> accu')
    @@ flow_source
    @ flow_sink
    @ extremal_labels
  ;;

  let solve x =
    let flowgraph = F.flow_of_t x
    and initials = F.extremal_labels_of_t x
    and extremal_value = L.extremal_value_of x
    and least_value = L.least_element_of x
    and assocs = F.associations_of_t x
    and all_props = TF.all_properties_of x in
    let init = initialize initials extremal_value least_value flowgraph
    and worklist = flowgraph in
    let analysis = solve_helper all_props flowgraph assocs init worklist in
    assocs, result_of_analysis all_props flowgraph assocs analysis
  ;;
end
