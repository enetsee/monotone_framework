open Core_kernel
open Lang
open Monotone_framework_lib

type t = Stmt.Labelled.t

module Property = Set.Make_using_comparator (String)

type property = Property.t

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

  let extremal_value_of (_ : t) = Property.empty
  let least_element_of _ = Property.empty
  let leq a b = Property.is_subset a ~of_:b
  let lub a b = Property.union a b
end

include Monotone_framework.Make (Stmt_flowgraph.Reverse) (L) (TF)
