open Core_kernel
open Lang
open Common

type t = Stmt.Labelled.t

module Label = Stmt.Labelled.Label
module LabelMap = Map.Make_using_comparator (Label)
module Property = Set.Make_using_comparator (Arith_expr.Labelled)

type property = Property.t

module KillGen :
  Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t
   and type property = Property.t = struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let gen { Stmt.Fixed.pattern; _ } =
    match pattern with
    | Stmt.Pattern.If (test, _, _) | Stmt.Pattern.While (test, _) ->
      Common.associate_bool_expr test
      |> Associations.arith_exprs
      |> List.filter_map ~f:(fun (_, aexp) ->
             if trivial aexp then None else Some aexp)
      |> Property.of_list
    | Assign (x, aexp) ->
      Common.associate_arith_expr aexp
      |> Associations.arith_exprs
      |> List.filter_map ~f:(fun (_, aexpr) ->
             if trivial aexpr
             then None
             else if List.exists ~f:(fun y -> x = y) @@ vars aexpr
             then None
             else Some aexpr)
      |> Property.of_list
    | _ -> Property.empty
  ;;

  let all_properties_of x =
    Common.associate x
    |> Associations.arith_exprs
    |> List.map ~f:snd
    |> Property.of_list
  ;;

  let kill prop_star { Stmt.Fixed.pattern; _ } =
    match pattern with
    | Assign (x, _) ->
      prop_star
      |> Property.filter ~f:(fun expr ->
             List.exists ~f:(fun y -> x = y) @@ vars expr)
    | _ -> Property.empty
  ;;

  let diff = Property.diff
  let union = Property.union
end

module TF = Transfer_function.Make_using_kill_gen (KillGen)

module L :
  Lattice.S with type t = Stmt.Labelled.t and type property = Property.t =
struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let least_element_of x =
    Common.associate x
    |> Associations.arith_exprs
    |> List.map ~f:snd
    |> Property.of_list
  ;;

  let extremal_value_of (_ : t) = Property.empty
  let leq a b = Property.is_subset b ~of_:a
  let lub a b = Property.inter a b
end

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)

let example_2_5 =
  Stmt.(
    Fixed.(
      block_
        [ assign_ "x" Arith_expr.Fixed.(plus_ (var_ "a") (var_ "b"))
        ; assign_ "y" Arith_expr.Fixed.(mult_ (var_ "a") (var_ "b"))
        ; while__
            Bool_expr.Fixed.(
              gt_
                (Arith_expr.Fixed.var_ "y")
                Arith_expr.Fixed.(plus_ (var_ "a") (var_ "b")))
            (block_
               [ assign_ "a" Arith_expr.Fixed.(plus_ (var_ "a") (lit_ 1))
               ; assign_ "x" Arith_expr.Fixed.(plus_ (var_ "a") (var_ "b"))
               ])
        ])
    |> Labelled.label)
;;
