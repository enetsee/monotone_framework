open Lang
open Core_kernel

type t = Stmt.Labelled.t
type property = Arith_expr.Labelled.t StringMap.t option

module L :
  Lattice.S
  with type t = Stmt.Labelled.t
   and type property = Arith_expr.Labelled.t StringMap.t option = struct
  type t = Stmt.Labelled.t
  type property = Arith_expr.Labelled.t StringMap.t option

  let lub_helper s1 s2 =
    let f ~key ~data =
      Map.find s2 key |> Option.value_map ~default:false ~f:(fun x -> x = data)
    in
    Map.filteri ~f s1
  ;;

  let lub p1 p2 =
    match Option.map2 ~f:lub_helper p1 p2 with
    | Some _ as x -> x
    | _ -> Option.first_some p1 p2
  ;;

  let extremal_value_of _ = Some StringMap.empty
  let least_element_of _ = None

  let leq_helper vs s1 s2 =
    List.for_all vs ~f:(fun k ->
        match Map.find s1 k, Map.find s2 k with
        | Some x, Some y -> x = y
        | Some _, None | None, None -> true
        | None, Some _ -> false)
  ;;

  let leq p1 p2 =
    match p1, p2 with
    | None, _ -> true
    | Some _, None -> false
    | Some m1, Some m2 ->
      let vs = StringMap.keys m1 @ StringMap.keys m2 in
      leq_helper vs m1 m2
  ;;
end

module TF :
  Transfer_function.S
  with type t = Stmt.Labelled.t
   and type property = Arith_expr.Labelled.t StringMap.t option = struct
  type t = Stmt.Labelled.t
  type property = Arith_expr.Labelled.t StringMap.t option

  let all_properties_of _ = None

  let is_literal { Arith_expr.Fixed.pattern; _ } =
    match pattern with
    | Lit _ -> true
    | _ -> false
  ;;

  let apply (_ : property) (t : t) (property : property) : property =
    Option.map property ~f:(fun env ->
        match Stmt.Fixed.pattern t with
        | Assign (vbl, aexpr) ->
          (match Partial_evaluation.eval_arith_expr ~env aexpr with
          | aexpr' when is_literal aexpr' ->
            StringMap.set env ~key:vbl ~data:aexpr'
          | _ -> StringMap.remove env vbl)
        | _ -> env)
  ;;
end

module F = Stmt_flowgraph.Forward
include Monotone_framework.Make (F) (L) (TF)

(** Substitute arithmetic expressions within a statement and partially evaluate *)
let apply_constants stmt env =
  Stmt.Fixed.trimap_pattern
    ~f:(fun m aexpr ->
      Arith_expr.Fixed.(
        fix m aexpr |> Partial_evaluation.eval_arith_expr ~env |> pattern))
    ~g:(fun _ x -> x)
    ~h:(fun _ x -> x)
    stmt
;;

(** Apply constant propagation, recovering the transformed statement *)
let apply (assocs, analysis) =
  assocs
  |> LabelMap.mapi ~f:(fun ~key ~data ->
         match
           LabelMap.find analysis key
           |> Option.bind ~f:Monotone_framework.entry
         with
         | Some env -> apply_constants data env
         | _ -> data)
  |> F.t_of_associations
;;

let optimize stmt = stmt |> solve |> apply |> Option.value ~default:stmt

let example =
  Stmt.Fixed.(
    block_
      [ assign_ "x" Arith_expr.Fixed.(plus_ (lit_ 2) (lit_ 2))
      ; assign_ "y" Arith_expr.Fixed.(mult_ (lit_ 1) (var_ "x"))
      ; assign_ "z" Arith_expr.Fixed.(lit_ 4)
      ; while__
          Bool_expr.Fixed.(
            gt_ Arith_expr.Fixed.(var_ "z") Arith_expr.Fixed.(var_ "y"))
          (block_
             [ assign_ "z" Arith_expr.Fixed.(minus_ (var_ "z") (lit_ 1))
             ; skip_
             ])
      ]
    |> Stmt.Labelled.label)
;;

let show_env (env : Arith_expr.Labelled.t StringMap.t) =
  StringMap.to_alist env
  |> List.map ~f:(fun (var, aexp) ->
         var, Fmt.to_to_string Arith_expr.Fixed.pp_ aexp)
;;

let show_property (prop : property) =
  Option.value_map ~default:[] ~f:show_env prop
;;

let show_solution
    (solution : property Monotone_framework.entry_exit LabelMap.t)
  =
  LabelMap.to_alist solution
  |> List.map ~f:(fun (lbl, { Monotone_framework.entry; exit }) ->
         lbl, (show_property entry, show_property exit))
;;

let show_entry (solution : property Monotone_framework.entry_exit LabelMap.t) =
  LabelMap.to_alist solution
  |> List.map ~f:(fun (lbl, { Monotone_framework.entry; _ }) ->
         lbl, show_property entry)
;;

let show_exit (solution : property Monotone_framework.entry_exit LabelMap.t) =
  LabelMap.to_alist solution
  |> List.map ~f:(fun (lbl, { Monotone_framework.exit; _ }) ->
         lbl, show_property exit)
;;
