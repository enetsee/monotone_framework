open Core_kernel
open Lang

type t = Stmt.Labelled.t


module Assignment : Lib.Label.S with type t = (string * int option) = struct
  module Basic = struct
    type t = string * int option [@@deriving compare, hash, sexp, sexp_of]
  end

  include Basic
  include Comparator.Make (Basic)
end

module Property = Set.Make_using_comparator(Assignment)

type property = Property.t

module Kill_gen : Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t 
   and type property = Property.t = struct

  type t = Stmt.Labelled.t 

  type property = Property.t

  let kill prop_star { Stmt.pattern; _ }  =
    match pattern with
    | Stmt.Pattern.Assign (x, _) ->
      Property.fold_right
        prop_star
        ~init:(Property.singleton (x, None))
        ~f:(fun (vbl,lbl_opt) accu  ->
          if vbl = x 
          then Property.add accu (vbl, lbl_opt)
          else accu)
    | _ -> Property.empty
  ;;

  let gen ({ Stmt.pattern; _ } as x) =
    match pattern with
    | Stmt.Pattern.Assign (var, _) ->
      Property.singleton (var, Some (Stmt.Labelled.label_of x))
    | _ -> Property.empty
  ;;

  let all_properties_of x =
    Common.associate x 
    |> Associations.stmts 
    |> List.filter_map 
        ~f:(fun (lbl,{pattern;_}) -> 
            match pattern with 
            | Stmt.Pattern.Assign(vbl,_) -> Some (vbl, Some lbl)
            | _ -> None
        )
    |> Property.of_list
  ;;

  let diff p1 p2  = Property.diff p1 p2

  let union  p1 p2 = Property.union  p1 p2

end 

module Transfer = 
  Transfer_function.Make_using_kill_gen(Kill_gen)


module Assignments_lattice : Lattice.S 
  with type t = Stmt.Labelled.t 
  and type property = Property.t = struct

  type t = Stmt.Labelled.t

  type property = Property.t
  let least_element_of _  = Property.empty
  let leq a b = Property.is_subset a ~of_:b
  let lub a b = Property.union a b
  let extremal_value_of (x : t) =
    Stmt.trifold_right_pattern
      ~f:(fun _ accu  -> accu)
      ~g:(fun _ accu  -> accu)
      ~h:(fun pattern accu -> 
            match pattern with 
            | Stmt.Pattern.Assign(vbl,_) -> 
              (vbl,None)::accu 
            | _ -> accu
          )
      ~init:[]
      x
    |> Property.of_list
end

include Monotone_framework.Make
  (Stmt_flowgraph.Forward)
  (Assignments_lattice)
  (Transfer)


let example_2_7 =
  Stmt.(
    block_
      [ assign_ "x" Arith_expr.(lit_ 5)
      ; assign_ "y" Arith_expr.(lit_ 1)
      ; while__
          Bool_expr.(
            gt_ (Arith_expr.var_ "x") Arith_expr.(lit_ 1))
          (block_
             [ assign_ "y" Arith_expr.(mult_ (var_ "x") (var_ "y"))
             ; assign_ "x" Arith_expr.(minus_ (var_ "x") (lit_ 1))
             ])
      ]
    |> Labelled.label)
;;



(* open Common *)

(* == Defined by `Stmt` =====================================================
type t = Stmt.Labelled.t

module Label = Stmt.Labelled.Label
module LabelSet = Set.Make_using_comparator (Label)
module LabelMap = Map.Make_using_comparator (Label)

(* == Defined by analysis =================================================== *)

module Property = struct
  module Basic = struct
    type t = string * int option [@@deriving compare, hash, sexp, sexp_of]
  end

  include Basic
  include Comparator.Make (Basic)
end

module PropertySet = Set.Make_using_comparator (Property)

type entry_exit_properties =
  { entry : PropertySet.t
  ; exit : PropertySet.t
  }

type result = entry_exit_properties LabelMap.t *)


(* let kill (blocks : Property.t list) ({ Stmt.pattern; _ } : t) =
  match pattern with
  | Stmt.Pattern.Assign (x, _) ->
    List.fold_left
      blocks
      ~init:(PropertySet.singleton (x, None))
      ~f:(fun accu (vbl,lbl_opt) ->
        if vbl = x 
        then PropertySet.add accu (vbl, lbl_opt)
        else accu)
  | _ -> PropertySet.empty
;;

let gen ({ Stmt.pattern; _ } as x : t) =
  match pattern with
  | Stmt.Pattern.Assign (var, _) ->
    PropertySet.singleton (var, Some (Stmt.Labelled.label_of x))
  | _ -> PropertySet.empty
;; *)

(* == Examples ============================================================== *)


(* let assocs_2_5 = associate example_2_5
let labels_2_5 : LabelSet.t = labels example_2_5
let blocks_2_5 = blocks assocs_2_5 labels_2_5
let initial_2_5 : Label.t = initial example_2_5
let flow_2_5 = flow example_2_5 *)

(* let kills = labels_2_5 |> IntSet.to_list |> List.filter_map ~f:(fun label ->
   Associations.find_stmt assocs_2_5 label |> Option.map ~f:(fun s -> label,
   kill labelled_blocks_2_5 s |> AExpSet.to_list) ) ;;

   let gens = labels_2_5 |> IntSet.to_list |> List.filter_map ~f:(fun label ->
   Associations.find_stmt assocs_2_5 label |> Option.map ~f:(fun s -> label,
   gen s |> AExpSet.to_list)) ;; *)

(* == Solve ================================================================= *)

(* 

module Monotone_framework = struct

  module Property = struct
    module Basic = struct
      type t = string * int option [@@deriving compare, hash, sexp, sexp_of]
    end

    include Basic
    include Comparator.Make (Basic)
  end

  module PropertySet = Set.Make_using_comparator (Property)
  (* == Definition of flowgraph =============================================== *)

  type t = Stmt.Labelled.t

  module Label = Stmt.Labelled.Label
  module LabelSet = Set.Make_using_comparator (Label)
  module LabelMap = Map.Make_using_comparator (Label)

  type flow = (Label.t * Label.t) list

  let flow_of (x : t) : flow = Common.flow x
  let extremal_labels_of (x : t) = LabelSet.singleton (Common.initial x)

    

  let associations x : t LabelMap.t =
    Common.associate x |> Associations.stmt_map
  ;;

  (* == Defined by lattice ==================================================== *)



  let least_element_of _ : PropertySet.t = PropertySet.empty
  let leq a b = PropertySet.is_subset a ~of_:b
  let lub a b = PropertySet.union a b

  let extremal_value_of (x : t) =
    Stmt.trifold_right_pattern
      ~f:(fun _ accu  -> accu)
      ~g:(fun _ accu  -> accu)
      ~h:(fun pattern accu -> 
            match pattern with 
            | Stmt.Pattern.Assign(vbl,_) -> (vbl,None)::accu 
            | _ -> accu
          )
      ~init:[]
      x
    |> PropertySet.of_list
  (* == Transfer functions ================================================== *)

  let kill (blocks : Property.t list) ({ Stmt.pattern; _ } : t) =
    match pattern with
    | Stmt.Pattern.Assign (x, _) ->
      List.fold_left
        blocks
        ~init:(PropertySet.singleton (x, None))
        ~f:(fun accu (vbl,lbl_opt) ->
          if vbl = x 
          then PropertySet.add accu (vbl, lbl_opt)
          else accu)
    | _ -> PropertySet.empty
  ;;

  let gen ({ Stmt.pattern; _ } as x : t) =
    match pattern with
    | Stmt.Pattern.Assign (var, _) ->
      PropertySet.singleton (var, Some (Stmt.Labelled.label_of x))
    | _ -> PropertySet.empty
  ;;

  let all_properties_of (x : t) =
    Common.associate x 
    |> Associations.stmts 
    |> List.filter_map 
        ~f:(fun (lbl,{pattern;_}) -> 
            match pattern with 
            | Stmt.Pattern.Assign(vbl,_) -> Some (vbl, Some lbl)
            | _ -> None
        )
  ;;

  let tf bottom t_map analysis label =
    match LabelMap.find t_map label, LabelMap.find analysis label with
    | Some stmt, Some aexps ->
      let kills = kill bottom stmt
      and gens = gen stmt in
      PropertySet.diff aexps kills |> PropertySet.union gens
    | _ -> failwith "can't happen"
  ;;


  (* == Common to all analyses ================================================ *)

  type entry_exit_properties =
    { entry : PropertySet.t
    ; exit : PropertySet.t
    }

  type result = entry_exit_properties LabelMap.t


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
end *)
