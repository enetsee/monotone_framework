open Core_kernel
open Lang
open Monotone_framework_lib

type t = Stmt.Labelled.t

module Property = Set.Make_using_comparator (String)

module KillGen :
  Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t
   and type property = Property.t = struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let kill _ { Stmt.Fixed.pattern; _ } =
    match pattern with
    | Assign (vbl, _) -> Property.singleton vbl
    | _ -> Property.empty
  ;;

  let gen { Stmt.Fixed.pattern; _ } =
    match pattern with
    | Assign (_, arith_expr) ->
      Arith_expr.Fixed.free_vars arith_expr |> Property.of_list
    | If (test, _, _) | While (test, _) ->
      Bool_expr.Fixed.free_vars test |> Property.of_list
    | _ -> Property.empty
  ;;

  let all_properties_of x =
    Stmt.Fixed.trifold_left_pattern
      x
      ~init:[]
      ~f:(fun accu _ _ -> accu)
      ~g:(fun accu _ _ -> accu)
      ~h:(fun accu _ pattern ->
        match pattern with
        | Assign (vbl, _) -> vbl :: accu
        | _ -> accu)
    |> Property.of_list
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

  let least_element_of _ = Property.empty
  let extremal_value_of (_ : t) = Property.empty
  let leq a b = Property.is_subset a ~of_:b
  let lub a b = Property.union a b
end

include Monotone_framework.Make (Stmt_flowgraph.Reverse) (L) (TF)

let example_2_10 =
  Stmt.(
    Fixed.(
      block_
        [ assign_ "x" Arith_expr.Fixed.(lit_ 2)
        ; assign_ "y" Arith_expr.Fixed.(lit_ 4)
        ; assign_ "x" Arith_expr.Fixed.(lit_ 1)
        ; if__
            Bool_expr.Fixed.(
              gt_ Arith_expr.Fixed.(var_ "y") Arith_expr.Fixed.(var_ "x"))
            (assign_ "z" Arith_expr.Fixed.(var_ "y"))
            (assign_ "z" Arith_expr.Fixed.(mult_ (var_ "y") (var_ "y")))
        ; assign_ "x" Arith_expr.Fixed.(var_ "z")
        ])
    |> Labelled.label)
;;
