open Core_kernel
open Lang
open Monotone_framework_lib

(* == Helpers =============================================================== *)
module LabelSet = Set.Make_using_comparator (Stmt.Labelled.Label)

(** Determine the set of labels for a labelled statement *)
let rec labels ?init:(accu = LabelSet.empty) stmt =
  let label = Stmt.Labelled.label_of stmt in
  labels_pattern label accu @@ Stmt.Fixed.pattern stmt

and labels_pattern cur_label accu = function
  | Stmt.Pattern.Assign _ | Skip -> LabelSet.add accu cur_label
  | If (_, s1, s2) ->
    labels ~init:(labels ~init:(LabelSet.add accu cur_label) s2) s1
  | While (_, body) -> labels ~init:(LabelSet.add accu cur_label) body
  | Block xs ->
    List.fold_right ~f:(fun x accu -> labels ~init:accu x) ~init:accu xs
;;

(** Get the lables of the initial statement in a statement *)
let rec initial stmt =
  let label = Stmt.Labelled.label_of stmt in
  initial_pattern label @@ Stmt.Fixed.pattern stmt

and initial_pattern cur_label = function
  | Stmt.Pattern.Assign _ | Skip | If (_, _, _) | While (_, _) -> cur_label
  | Block xs -> initial @@ List.hd_exn xs
;;

(** Get the labels of the possible final statements in a statement *)
let rec finals ?init:(accu = LabelSet.empty) stmt =
  let label = Stmt.Labelled.label_of stmt in
  final_pattern label accu @@ Stmt.Fixed.pattern stmt

and final_pattern cur_label accu = function
  | Stmt.Pattern.Assign _ | Skip -> LabelSet.add accu cur_label
  | If (_, ts, fs) -> finals ~init:(finals ~init:accu fs) ts
  | While (_, _) -> LabelSet.add accu cur_label
  | Block xs ->
    (match List.last xs with
    | Some x -> finals ~init:accu x
    | _ -> accu)
;;

(** Build the flowgraph of a statment in terms of `initial` and `finals` *)
let rec flow ?init:(accu = []) stmt =
  let label = Stmt.Labelled.label_of stmt in
  flow_pattern label accu @@ Stmt.Fixed.pattern stmt

and flow_pattern cur_label accu = function
  | Stmt.Pattern.Assign _ | Skip -> accu
  | If (_, ts, fs) ->
    let init_ts = initial ts
    and init_fs = initial fs in
    flow
      ~init:
        (flow ~init:((cur_label, init_ts) :: (cur_label, init_fs) :: accu) fs)
      ts
  | While (_, body) ->
    let init_body = initial body
    and finals_body = finals body in
    let finals =
      List.map ~f:(fun l' -> l', cur_label) @@ LabelSet.to_list finals_body
    in
    flow ~init:(((cur_label, init_body) :: finals) @ accu) body
  | Block xs -> pairwise ~init:accu xs

and pairwise ~init = function
  | x :: y :: xs ->
    let init_y = initial y in
    let pairs =
      finals x |> LabelSet.to_list |> List.map ~f:(fun l -> l, init_y)
    in
    pairwise ~init:(flow ~init:(init @ pairs) x) (y :: xs)
  | [ x ] -> flow ~init x
  | [] -> init
;;

(** Subsitute statements within a statement *)
let subst_stmt stmt env =
  Stmt.Fixed.trimap_pattern
    ~f:(fun _ x -> x)
    ~g:(fun _ x -> x)
    ~h:(fun { Stmt.Labelled.label } s ->
      Stmt.Labelled.LabelMap.find env label
      |> Option.value_map ~default:s ~f:Stmt.Fixed.pattern)
    stmt
;;

(** Recover a statement from it's flowgraph *)
let recover assocs =
  Stmt.Labelled.LabelMap.fold_right
    assocs
    ~f:(fun ~key ~data accu_opt ->
      match accu_opt with
      | Some (_, env) ->
        let stmt' = subst_stmt data env in
        let env' = Stmt.Labelled.LabelMap.add_exn env ~key ~data in
        Some (stmt', env')
      | _ ->
        Some
          ( data
          , Stmt.Labelled.LabelMap.add_exn
              Stmt.Labelled.LabelMap.empty
              ~key
              ~data ))
    ~init:None
  |> Option.map ~f:fst
;;

(* == Flowgraph ============================================================= *)

module Stmt_flow :
  Flowgraph.Basic
  with type t = Stmt.Labelled.t
   and module Label = Stmt.Labelled.Label = struct
  type t = Stmt.Labelled.t

  module Label = Stmt.Labelled.Label
  module LabelSet = Set.Make_using_comparator (Label)

  let initial_label_of_t x = initial x
  let final_labels_of_t x = finals x
  let flow_of_t x = flow x
  let t_of_associations f = recover f

  let associations_of_t x =
    let { Stmt.Labelled.stmts; _ } = Stmt.Labelled.associate x in
    stmts
  ;;
end

module Forward = Flowgraph.Make (Stmt_flow)
module Reverse = Flowgraph.Make_reverse (Stmt_flow)
