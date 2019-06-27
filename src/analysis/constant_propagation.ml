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

  let merge p1 p2 =
    Map.filteri p1 ~f:(fun ~key ~data ->
        Map.find p2 key
        |> Option.value_map ~default:false ~f:(fun x -> x = data))
  ;;

  let lub p1 p2 =
    match Option.map2 p1 p2 ~f:merge with
    | Some _ as x -> x
    | _ -> Option.first_some p1 p2
  ;;

  let extremal_value_of _ = Some StringMap.empty
  let least_element_of _ = None

  let leq p1 p2 =
    match p1, p2 with
    | None, _ -> true
    | Some _, None -> true
    | Some m1, Some m2 ->
      StringMap.for_alli m1 ~f:(fun ~key ~data ->
          StringMap.find m2 key
          |> Option.value_map ~default:true ~f:(fun x -> x = data))
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

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)

let example =
  Stmt.Fixed.(
    block_
      [ assign_ "x" Arith_expr.Fixed.(plus_ (lit_ 2) (lit_ 2))
      ; assign_ "y" Arith_expr.Fixed.(mult_ (lit_ 1) (var_ "x"))
      ; assign_ "z" Arith_expr.Fixed.(lit_ 4)
      ; while__
          Bool_expr.Fixed.(
            gt_ Arith_expr.Fixed.(var_ "z") Arith_expr.Fixed.(var_ "y"))
          (block_ [ skip_; skip_ ])
      ]
    |> Stmt.Labelled.label)
;;
