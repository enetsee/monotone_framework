open Core_kernel
open Lang
open Common

(* == Defined by `Stmt` ===================================================== *)

type t = Stmt.Labelled.t

module Label = Stmt.Labelled.Label
module LabelSet = Set.Make_using_comparator (Label)
module LabelMap = Map.Make_using_comparator (Label)

(* == Definition of flowgraph =============================================== *)

type flow = (Label.t * Label.t) list

(* == Defined by analysis =================================================== *)

module Property = Arith_expr.Labelled
module PropertySet = Set.Make_using_comparator (Property)

(* == Common to all analyses ================================================ *)

type entry_exit_properties =
  { entry : PropertySet.t
  ; exit : PropertySet.t
  }

type result = entry_exit_properties LabelMap.t

let kill aexp_star ({ Stmt.pattern; _ } : t) =
  match pattern with
  | Stmt.Pattern.Assign (x, _) ->
    aexp_star
    |> PropertySet.filter ~f:(fun expr ->
           List.exists ~f:(fun y -> x = y) @@ vars expr)
  | _ -> PropertySet.empty
;;

let gen ({ Stmt.pattern; _ } : t) =
  match pattern with
  | Stmt.Pattern.If (test, _, _) | Stmt.Pattern.While (test, _) ->
    Common.associate_bool_expr test
    |> Associations.arith_exprs
    |> List.filter_map ~f:(fun (_, aexp) ->
           if trivial aexp then None else Some aexp)
    |> PropertySet.of_list
  | Assign (x, aexp) ->
    Common.associate_arith_expr aexp
    |> Associations.arith_exprs
    |> List.filter_map ~f:(fun (_, aexpr) ->
           if trivial aexpr
           then None
           else if List.exists ~f:(fun y -> x = y) @@ vars aexpr
           then None
           else Some aexpr)
    |> PropertySet.of_list
  | _ -> PropertySet.empty
;;

(* == Examples ============================================================== *)
let example_2_5 =
  Stmt.(
    block_
      [ assign_ "x" Arith_expr.(plus_ (var_ "a") (var_ "b"))
      ; assign_ "y" Arith_expr.(mult_ (var_ "a") (var_ "b"))
      ; while__
          Bool_expr.(
            gt_ (Arith_expr.var_ "y") Arith_expr.(plus_ (var_ "a") (var_ "b")))
          (block_
             [ assign_ "a" Arith_expr.(plus_ (var_ "a") (lit_ 1))
             ; assign_ "x" Arith_expr.(plus_ (var_ "a") (var_ "b"))
             ])
      ]
    |> Labelled.label)
;;

let assocs_2_5 = associate example_2_5
let labels_2_5 = labels example_2_5
let blocks_2_5 = blocks assocs_2_5 labels_2_5
let aexp_2_5 = non_trivial_arith_expr assocs_2_5
let initial_2_5 = initial example_2_5
let flow_2_5 : flow = flow example_2_5

let kills =
  labels_2_5
  |> LabelSet.to_list
  |> List.filter_map ~f:(fun label ->
         Associations.find_stmt assocs_2_5 label
         |> Option.map ~f:(fun s -> label, kill aexp_2_5 s |> AExpSet.to_list)
     )
;;

let gens =
  labels_2_5
  |> LabelSet.to_list
  |> List.filter_map ~f:(fun label ->
         Associations.find_stmt assocs_2_5 label
         |> Option.map ~f:(fun s -> label, gen s |> AExpSet.to_list))
;;

(* == Solve ================================================================= *)

let e = LabelSet.singleton initial_2_5
let f : flow = flow_2_5
let leq a b = PropertySet.is_subset b ~of_:a
let lub a b = PropertySet.inter a b
let w : (Label.t * Label.t) list = f
let i = PropertySet.empty
let bottom = aexp_2_5

type props =
  { flow : (Label.t * Label.t) list
  ; i : PropertySet.t
  ; bottom : PropertySet.t
  ; initial_label : Label.t
  }

let tf bottom assocs analysis label =
  match Associations.find_stmt assocs label, LabelMap.find analysis label with
  | Some stmt, Some aexps ->
    let kills = kill bottom stmt
    and gens = gen stmt in
    PropertySet.diff aexps kills |> PropertySet.union gens
  | _ -> failwith "can't happen"
;;

let rec enqueue label accu = function
  | [] -> accu
  | (l, l') :: rest when l = label -> enqueue label ((l, l') :: accu) rest
  | _ :: rest -> enqueue label accu rest
;;

let update_analysis analysis (label : Label.t) xs =
  LabelMap.update analysis label ~f:(fun aexp_opt ->
      aexp_opt |> Option.value ~default:AExpSet.empty |> lub xs)
;;

let rec aux flow analysis = function
  | [] -> analysis
  | (l, l') :: rest ->
    let tf_l = tf aexp_2_5 assocs_2_5 analysis l
    and aexp_l' = LabelMap.find_exn analysis l' in
    if not (leq tf_l aexp_l')
    then (
      let analysis' = update_analysis analysis l' tf_l
      and w = enqueue l' rest flow in
      aux flow analysis' w)
    else aux flow analysis rest
;;

let entry_of_analysis analysis =
  List.map ~f:fst f
  |> List.map ~f:(fun lbl -> lbl, LabelMap.find_exn analysis lbl)
  |> LabelMap.of_alist_exn
;;

let exit_of_analysis bottom assocs analysis =
  List.map ~f:fst f
  |> List.map ~f:(fun l ->
         let tf_l = tf bottom assocs analysis l in
         l, tf_l)
  |> LabelMap.of_alist_exn
;;

let analysis =
  LabelMap.of_alist_exn
    (List.map ~f:(fun (l, _) -> l, if l = initial_2_5 then i else bottom) f)
;;

let analysis' = aux f analysis w

let entry =
  List.map ~f:fst f
  |> List.map ~f:(fun lbl -> lbl, LabelMap.find_exn analysis' lbl)
  |> LabelMap.of_alist_exn
;;

let entry_visible =
  List.map ~f:fst f
  |> List.sort ~compare:(fun x y -> x - y)
  |> List.map ~f:(fun lbl ->
         lbl, LabelMap.find_exn analysis' lbl |> AExpSet.to_list)
;;

let exit =
  List.map ~f:fst f
  |> List.map ~f:(fun l ->
         let tf_l = tf aexp_2_5 assocs_2_5 analysis' l in
         l, tf_l)
  |> LabelMap.of_alist_exn
;;

let exit_visible =
  List.map ~f:fst f
  |> List.sort ~compare:(fun x y -> x - y)
  |> List.map ~f:(fun l ->
         let tf_l = tf aexp_2_5 assocs_2_5 analysis' l in
         l, AExpSet.to_list tf_l)
;;

module Monotone_framework = struct
  (* == Definition of flowgraph =============================================== *)

  type t = Stmt.Labelled.t

  module Label = Stmt.Labelled.Label
  module LabelSet = Set.Make_using_comparator (Label)
  module LabelMap = Map.Make_using_comparator (Label)

  type flow = (Label.t * Label.t) list

  let flow_of (x : t) : flow = Common.flow x
  let extremal_labels_of (x : t) = LabelSet.singleton (Common.initial x)
  let extremal_value_of (_ : t) = PropertySet.empty

  let associations x : t LabelMap.t =
    Common.associate x |> Associations.stmt_map
  ;;

  (* == Defined by lattice ==================================================== *)

  module Property = Arith_expr.Labelled
  module PropertySet = Set.Make_using_comparator (Property)

  let least_element_of _ : PropertySet.t = aexp_2_5
  let leq a b = PropertySet.is_subset b ~of_:a
  let lub a b = PropertySet.inter a b

  (* == Transfer functions ================================================== *)

  let gen ({ Stmt.pattern; _ } : t) =
    match pattern with
    | Stmt.Pattern.If (test, _, _) | Stmt.Pattern.While (test, _) ->
      Common.associate_bool_expr test
      |> Associations.arith_exprs
      |> List.filter_map ~f:(fun (_, aexp) ->
             if trivial aexp then None else Some aexp)
      |> PropertySet.of_list
    | Assign (x, aexp) ->
      Common.associate_arith_expr aexp
      |> Associations.arith_exprs
      |> List.filter_map ~f:(fun (_, aexpr) ->
             if trivial aexpr
             then None
             else if List.exists ~f:(fun y -> x = y) @@ vars aexpr
             then None
             else Some aexpr)
      |> PropertySet.of_list
    | _ -> PropertySet.empty
  ;;

  let all_properties_of (x : t) =
    Common.associate x |> Associations.arith_exprs |> List.map ~f:snd
  ;;

  let kill prop_star ({ Stmt.pattern; _ } : t) =
    match pattern with
    | Stmt.Pattern.Assign (x, _) ->
      prop_star
      |> List.filter ~f:(fun expr ->
             List.exists ~f:(fun y -> x = y) @@ vars expr)
      |> PropertySet.of_list
    | _ -> PropertySet.empty
  ;;

  (* == Common to all analyses ================================================ *)

  type entry_exit_properties =
    { entry : PropertySet.t
    ; exit : PropertySet.t
    }

  type result = entry_exit_properties LabelMap.t

  let tf bottom t_map analysis label =
    match LabelMap.find t_map label, LabelMap.find analysis label with
    | Some stmt, Some aexps ->
      let kills = kill bottom stmt
      and gens = gen stmt in
      PropertySet.diff aexps kills |> PropertySet.union gens
    | _ -> failwith "can't happen"
  ;;

  let rec enqueue label accu = function
    | [] -> accu
    | (l, l') :: rest when l = label -> enqueue label ((l, l') :: accu) rest
    | _ :: rest -> enqueue label accu rest
  ;;

  let update_analysis analysis (label : Label.t) xs =
    LabelMap.update analysis label ~f:(fun aexp_opt ->
        aexp_opt |> Option.value ~default:PropertySet.empty |> lub xs)
  ;;

  let rec aux all_props flow assocs analysis = function
    | [] -> analysis
    | (l1, l2) :: rest ->
      let tf_l1 = tf all_props assocs analysis l1
      and aexp_l2 = LabelMap.find_exn analysis l2 in
      if not (leq tf_l1 aexp_l2)
      then (
        let analysis' = update_analysis analysis l2 tf_l1
        and w = enqueue l2 rest flow in
        aux all_props flow assocs analysis' w)
      else aux all_props flow assocs analysis rest
  ;;

  let result_of_analysis all_props flow assocs analysis =
    List.map ~f:fst flow
    |> List.map ~f:(fun lbl ->
           let entry = LabelMap.find_exn analysis lbl
           and exit = tf all_props assocs analysis lbl in
           lbl, { entry; exit })
    |> LabelMap.of_alist_exn
  ;;

  let initialise
      (initials : LabelSet.t)
      (extremal_value : PropertySet.t)
      (least_value : PropertySet.t)
      ((label, _) : Label.t * Label.t)
    =
    if LabelSet.mem initials label
    then label, extremal_value
    else label, least_value
  ;;

  let mfp (x : t) =
    let flowgraph = flow_of x
    and initials = extremal_labels_of x
    and extremal_value = extremal_value_of x
    and least_value = least_element_of x
    and assocs = associations x
    and all_props = all_properties_of x in
    let worklist = flowgraph in
    let init : PropertySet.t LabelMap.t =
      List.map ~f:(initialise initials extremal_value least_value) flowgraph
      |> LabelMap.of_alist_exn
    in
    let analysis = aux all_props flowgraph assocs init worklist in
    result_of_analysis all_props flowgraph assocs analysis
  ;;
end
