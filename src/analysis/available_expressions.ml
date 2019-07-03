open Core_kernel
open Lang
open Monotone_framework_lib

type t = Stmt.Labelled.t

module Label = Stmt.Labelled.Label
module LabelMap = Stmt.Labelled.LabelMap
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
      Bool_expr.Fixed.bifold_left_pattern
        test
        ~init:[]
        ~f:(fun accu meta pattern ->
          if Arith_expr.Pattern.is_trivial pattern
          then accu
          else Arith_expr.Fixed.fix meta pattern :: accu)
        ~g:(fun accu _ _ -> accu)
      |> Property.of_list
    | Assign (x, aexp) ->
      Arith_expr.Fixed.fold_left_pattern
        ~init:[]
        aexp
        ~f:(fun accu meta pattern ->
          if Arith_expr.Pattern.is_trivial pattern
          then accu
          else (
            let aexpr = Arith_expr.Fixed.fix meta pattern in
            let free_vars = Arith_expr.Fixed.free_vars aexpr in
            if List.mem free_vars x ~equal:String.equal
            then accu
            else aexpr :: accu))
      |> Property.of_list
    | _ -> Property.empty
  ;;

  let all_properties_of stmt =
    Stmt.Fixed.trifold_left_pattern
      stmt
      ~init:[]
      ~f:(fun accu meta pattern -> Arith_expr.Fixed.fix meta pattern :: accu)
      ~g:(fun accu _ _ -> accu)
      ~h:(fun accu _ _ -> accu)
    |> Property.of_list
  ;;

  let kill prop_star { Stmt.Fixed.pattern; _ } =
    match pattern with
    | Assign (x, _) ->
      prop_star
      |> Property.filter ~f:(fun expr ->
             let free_vars = Arith_expr.Fixed.free_vars expr in
             List.mem free_vars x ~equal:String.equal)
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
    Stmt.Fixed.trifold_left_pattern
      x
      ~init:[]
      ~f:(fun accu meta pattern -> Arith_expr.Fixed.fix meta pattern :: accu)
      ~g:(fun accu _ _ -> accu)
      ~h:(fun accu _ _ -> accu)
    |> Property.of_list
  ;;

  let extremal_value_of (_ : t) = Property.empty
  let leq a b = Property.is_subset b ~of_:a
  let lub a b = Property.inter a b
end

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)
