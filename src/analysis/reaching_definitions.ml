open Core_kernel
open Lang

type t = Stmt.Labelled.t

module Assignment : Lib.Label.S with type t = string * int option = struct
  module Basic = struct
    type t = string * int option [@@deriving compare, hash, sexp, sexp_of]
  end

  include Basic
  include Comparator.Make (Basic)
end

module Property = Set.Make_using_comparator (Assignment)

type property = Property.t

module Kill_gen :
  Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t
   and type property = Property.t = struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let kill prop_star { Stmt.Fixed.pattern; _ } =
    match pattern with
    | Stmt.Pattern.Assign (x, _) ->
      Property.fold_right
        prop_star
        ~init:(Property.singleton (x, None))
        ~f:(fun (vbl, lbl_opt) accu ->
          if vbl = x then Property.add accu (vbl, lbl_opt) else accu)
    | _ -> Property.empty
  ;;

  let gen ({ Stmt.Fixed.pattern; _ } as x) =
    match pattern with
    | Stmt.Pattern.Assign (var, _) ->
      Property.singleton (var, Some (Stmt.Labelled.label_of x))
    | _ -> Property.empty
  ;;

  let all_properties_of x =
    Common.associate x
    |> Associations.stmts
    |> List.filter_map ~f:(fun (lbl, { pattern; _ }) ->
           match pattern with
           | Stmt.Pattern.Assign (vbl, _) -> Some (vbl, Some lbl)
           | _ -> None)
    |> Property.of_list
  ;;

  let diff p1 p2 = Property.diff p1 p2
  let union p1 p2 = Property.union p1 p2
end

module Transfer = Transfer_function.Make_using_kill_gen (Kill_gen)

module Assignments_lattice :
  Lattice.S with type t = Stmt.Labelled.t and type property = Property.t =
struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let least_element_of _ = Property.empty
  let leq a b = Property.is_subset a ~of_:b
  let lub a b = Property.union a b

  let extremal_value_of (x : t) =
    Stmt.Fixed.trifold_right_pattern
      ~f:(fun _ accu -> accu)
      ~g:(fun _ accu -> accu)
      ~h:(fun pattern accu ->
        match pattern with
        | Stmt.Pattern.Assign (vbl, _) -> (vbl, None) :: accu
        | _ -> accu)
      ~init:[]
      x
    |> Property.of_list
  ;;
end

include Monotone_framework.Make (Stmt_flowgraph.Forward) (Assignments_lattice)
          (Transfer)

let example_2_7 =
  Stmt.(
    Fixed.(
      block_
        [ assign_ "x" Arith_expr.Fixed.(lit_ 5)
        ; assign_ "y" Arith_expr.Fixed.(lit_ 1)
        ; while__
            Bool_expr.Fixed.(
              gt_ (Arith_expr.Fixed.var_ "x") Arith_expr.Fixed.(lit_ 1))
            (block_
               [ assign_ "y" Arith_expr.Fixed.(mult_ (var_ "x") (var_ "y"))
               ; assign_ "x" Arith_expr.Fixed.(minus_ (var_ "x") (lit_ 1))
               ])
        ])
    |> Labelled.label)
;;
