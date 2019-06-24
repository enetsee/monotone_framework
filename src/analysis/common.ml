open Core_kernel
open Lang

(** Construct maps from labels to expressions and statements *)
let rec associate ?init:(accu = Associations.empty) (stmt : Stmt.Labelled.t) =
  let accu' = Associations.add_stmt accu stmt in
  associate_pattern accu' @@ Stmt.pattern stmt

and associate_pattern accu = function
  | Stmt.Pattern.Assign (_, a) -> associate_arith_expr ~init:accu a
  | Skip -> accu
  | If (pred, t, f) ->
    associate_bool_expr ~init:(associate ~init:(associate ~init:accu t) f) pred
  | While (pred, body) ->
    associate_bool_expr ~init:(associate ~init:accu body) pred
  | Block xs ->
    List.fold_right ~f:(fun x accu -> associate ~init:accu x) ~init:accu xs

and associate_bool_expr
    ?init:(accu = Associations.empty) (bool_expr : Bool_expr.Labelled.t)
  =
  let accu' = Associations.add_bool_expr accu bool_expr in
  associate_bool_expr_pattern accu' @@ Bool_expr.pattern bool_expr

and associate_bool_expr_pattern accu = function
  | True | False -> accu
  | Not b -> associate_bool_expr ~init:accu b
  | Boolop (b1, _, b2) ->
    associate_bool_expr ~init:(associate_bool_expr ~init:accu b2) b1
  | Relop (a1, _, a2) ->
    associate_arith_expr ~init:(associate_arith_expr ~init:accu a2) a1

and associate_arith_expr
    ?init:(accu = Associations.empty) (arith_expr : Arith_expr.Labelled.t)
  =
  Associations.add_arith_expr
    (associate_arith_expr_pattern accu @@ Arith_expr.pattern arith_expr)
    arith_expr

and associate_arith_expr_pattern accu = function
  | Lit _ | Var _ -> accu
  | Binop (a1, _, a2) ->
    associate_arith_expr ~init:(associate_arith_expr ~init:accu a2) a1
;;

module StmtLabelSet = Set.Make_using_comparator (Stmt.Labelled.Label)

(** Determine the set of labels for a labelled statement *)
let rec labels ?init:(accu = StmtLabelSet.empty) stmt =
  let label = Stmt.Labelled.label_of stmt in
  labels_pattern label accu @@ Stmt.pattern stmt

and labels_pattern cur_label accu = function
  | Stmt.Pattern.Assign _ | Skip -> StmtLabelSet.add accu cur_label
  | If (_, s1, s2) ->
    labels ~init:(labels ~init:(StmtLabelSet.add accu cur_label) s2) s1
  | While (_, body) -> labels ~init:(StmtLabelSet.add accu cur_label) body
  | Block xs ->
    List.fold_right ~f:(fun x accu -> labels ~init:accu x) ~init:accu xs
;;

(** Retrieve the statments corresponding to the `labels` of a statement! *)
let blocks assocs labels =
  StmtLabelSet.to_list labels
  |> List.filter_map ~f:(fun label -> Associations.find_stmt assocs label)
;;

let labelled_blocks assocs labels =
  StmtLabelSet.to_list labels
  |> List.filter_map ~f:(fun label ->
         Option.map ~f:(fun x -> label, x)
         @@ Associations.find_stmt assocs label)
;;

(** Get the lables of the initial statement in a statement *)
let rec initial stmt =
  let label = Stmt.Labelled.label_of stmt in
  initial_pattern label @@ Stmt.pattern stmt

and initial_pattern cur_label = function
  | Stmt.Pattern.Assign _ | Skip | If (_, _, _) | While (_, _) -> cur_label
  | Block xs -> initial @@ List.hd_exn xs
;;

(** Get the labels of the possible final statements in a statement *)
let rec finals ?init:(accu = StmtLabelSet.empty) stmt =
  let label = Stmt.Labelled.label_of stmt in
  final_pattern label accu @@ Stmt.pattern stmt

and final_pattern cur_label accu = function
  | Stmt.Pattern.Assign _ | Skip -> StmtLabelSet.add accu cur_label
  | If (_, ts, fs) -> finals ~init:(finals ~init:accu fs) ts
  | While (_, _) -> StmtLabelSet.add accu cur_label
  | Block xs ->
    (match List.last xs with
    | Some x -> finals ~init:accu x
    | _ -> accu)
;;

(** Build the flowgraph of a statment in terms of `initial` and `finals` *)
let rec flow ?init:(accu = []) stmt =
  let label = Stmt.Labelled.label_of stmt in
  flow_pattern label accu @@ Stmt.pattern stmt

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
      List.map ~f:(fun l' -> l', cur_label) @@ StmtLabelSet.to_list finals_body
    in
    flow ~init:(((cur_label, init_body) :: finals) @ accu) body
  | Block xs -> pairwise ~init:accu xs

and pairwise ~init = function
  | x :: y :: xs ->
    let init_y = initial y in
    let pairs =
      finals x |> StmtLabelSet.to_list |> List.map ~f:(fun l -> l, init_y)
    in
    pairwise ~init:(flow ~init:(init @ pairs) x) (y :: xs)
  | [ x ] -> flow ~init x
  | [] -> init
;;

(** The variables in an arithmetic expression *)
let vars ?init:(accu = []) expr =
  let f accu = function
    | Arith_expr.Pattern.Var n -> n :: accu
    | _ -> accu
  in
  Arith_expr.fold_left_pattern ~f ~init:accu expr
;;

let vars_pattern ?init:(accu = []) expr =
  Arith_expr.Pattern.fold_right
    ~f:(fun x accu -> vars ~init:accu x)
    ~init:accu
    expr
;;

(** The non-trivial arithmetic expressions in a statement *)
let trivial { Arith_expr.pattern; _ } =
  match pattern with
  | Arith_expr.Pattern.Var _ | Lit _ -> true
  | _ -> false
;;

module AExpSet = Set.Make_using_comparator (Arith_expr.Labelled)

let non_trivial_arith_expr assocs =
  Associations.arith_exprs assocs
  |> List.filter ~f:(fun (_, x) -> not @@ trivial x)
  |> List.map ~f:snd
  |> AExpSet.of_list
;;
