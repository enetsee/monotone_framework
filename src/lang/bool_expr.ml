open Core_kernel
open Lib
open State.Cps

type bool_op =
  | And
  | Or
[@@deriving compare, sexp, hash]

type rel_op =
  | Eq
  | Gt
  | Lt
[@@deriving compare, sexp, hash]

module Pattern : sig
  type ('a, 'b) t =
    | True
    | False
    | Not of 'b
    | Boolop of 'b * bool_op * 'b
    | Relop of 'a * rel_op * 'a

  include Pattern_functor.S2 with type ('a, 'b) t := ('a, 'b) t

  val true_ : ('a, 'b) t
  val false_ : ('a, 'b) t
  val not_ : 'b -> ('a, 'b) t
  val and_ : 'b -> 'b -> ('a, 'b) t
  val or_ : 'b -> 'b -> ('a, 'b) t
  val eq : 'a -> 'a -> ('a, 'b) t
  val gt : 'a -> 'a -> ('a, 'b) t
  val lt : 'a -> 'a -> ('a, 'b) t
end = struct
  module Basic = struct
    type ('a, 'b) t =
      | True
      | False
      | Not of 'b
      | Boolop of 'b * bool_op * 'b
      | Relop of 'a * rel_op * 'a
    [@@deriving map, fold, compare, sexp, hash]

    let bimap ~f ~g x = map f g x
    let bifold_left ~f ~g ~init x = fold f g init x
    let bifold_right = `Define_using_bifold_left
  end

  include Basic
  include Bifunctor.Make (Basic)
  include Bifoldable.Make (Basic)

  let true_ = True
  let false_ = False
  let not_ a = Not a
  let and_ a b = Boolop (a, And, b)
  let or_ a b = Boolop (a, Or, b)
  let eq a b = Relop (a, Eq, b)
  let gt a b = Relop (a, Gt, b)
  let lt a b = Relop (a, Lt, b)
end

module Fixed = Fix.Make2 (Pattern) (Arith_expr)
include Fixed

(* == Constructors ========================================================== *)
let true_ meta = fix meta Pattern.true_
let true__ : (unit, unit) t = true_ ()
let false_ meta = fix meta Pattern.false_
let false__ : (unit, unit) t = false_ ()
let not_ meta a = fix meta @@ Pattern.not_ a
let not__ a = not_ () a
let and_ meta a b = fix meta @@ Pattern.and_ a b
let and__ a b = and_ () a b
let or_ meta a b = fix meta @@ Pattern.or_ a b
let or__ a b = or_ () a b
let eq meta a b = fix meta @@ Pattern.eq a b
let eq_ a b = eq () a b
let gt meta a b = fix meta @@ Pattern.gt a b
let gt_ a b = gt () a b
let lt meta a b = fix meta @@ Pattern.lt a b
let lt_ a b = lt () a b

(* == Traversable Functors ================================================== *)

module Make_bitraversable_pattern (M : Monad.S) :
  Bitraversable.S with module M := M and module F := Pattern = struct
  let bitraverse ~f ~g = function
    | Pattern.True -> M.return @@ Pattern.True
    | False -> M.return @@ Pattern.False
    | Not e -> M.map ~f:(fun e -> Pattern.Not e) (g e)
    | Boolop (e1, op, e2) ->
      M.(
        g e1
        >>= fun e1' ->
        g e2 >>= fun e2' -> return @@ Pattern.Boolop (e1', op, e2'))
    | Pattern.Relop (e1, op, e2) ->
      M.(
        f e1
        >>= fun e1' ->
        f e2 >>= fun e2' -> return @@ Pattern.Relop (e1', op, e2'))
  ;;
end

module Make_bitraversable_pattern2 (M : Monad.S2) :
  Bitraversable.S2 with module M := M and module F := Pattern = struct
  let bitraverse ~f ~g = function
    | Pattern.True -> M.return @@ Pattern.True
    | False -> M.return @@ Pattern.False
    | Not e -> M.map ~f:(fun e -> Pattern.Not e) (g e)
    | Boolop (e1, op, e2) ->
      M.(
        g e1
        >>= fun e1' ->
        g e2 >>= fun e2' -> return @@ Pattern.Boolop (e1', op, e2'))
    | Pattern.Relop (e1, op, e2) ->
      M.(
        f e1
        >>= fun e1' ->
        f e2 >>= fun e2' -> return @@ Pattern.Relop (e1', op, e2'))
  ;;
end

module Make_bitraversable (M : Monad.S) :
  Bitraversable.S with module M := M and module F := Fixed = struct
  module BitraversablePattern = Make_bitraversable_pattern (M)
  module TraversableArithExpr = Arith_expr.Make_traversable (M)

  let bitraverse_pattern = BitraversablePattern.bitraverse
  let traverse_arith_expr = TraversableArithExpr.traverse

  let rec bitraverse ~f ~g { pattern; meta } =
    M.(
      g meta
      >>= fun meta' ->
      bitraverse_pattern
        ~f:(traverse_arith_expr ~f)
        ~g:(bitraverse ~f ~g)
        pattern
      |> map ~f:(fix meta'))
  ;;
end

module Make_bitraversable2 (M : Monad.S2) :
  Bitraversable.S2 with module M := M and module F := Fixed = struct
  module BitraversablePattern = Make_bitraversable_pattern2 (M)
  module TraversableArithExpr = Arith_expr.Make_traversable2 (M)

  let bitraverse_pattern = BitraversablePattern.bitraverse
  let traverse_arith_expr = TraversableArithExpr.traverse

  let rec bitraverse ~f ~g { pattern; meta } =
    M.(
      g meta
      >>= fun meta' ->
      bitraverse_pattern
        ~f:(traverse_arith_expr ~f)
        ~g:(bitraverse ~f ~g)
        pattern
      |> map ~f:(fix meta'))
  ;;
end

module Bitraversable_state = Make_bitraversable2 (State)

let bitraverse_state = Bitraversable_state.bitraverse

(* == Boolean expressions with no meta-data ==================================*)

module Unlabelled = struct
  type meta = unit [@@deriving compare, hash, sexp]
  type nonrec t = (Arith_expr.Unlabelled.meta, meta) t

  let compare x = compare Arith_expr.Unlabelled.compare_meta compare_meta x
  let sexp_of_t x = sexp_of_t Arith_expr.Unlabelled.sexp_of_meta sexp_of_meta x
  let t_of_sexp x = t_of_sexp Arith_expr.Unlabelled.meta_of_sexp meta_of_sexp x

  let hash_fold_t =
    hash_fold_t Arith_expr.Unlabelled.hash_fold_meta hash_fold_meta
  ;;

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let unlabel bool_expr : t = bimap ~f:(fun _ -> ()) ~g:(fun _ -> ()) bool_expr
end

(* == Integer labelled boolean expressions ================================== *)

module Labelled = struct
  type meta = { label : int } [@@deriving compare, hash, sexp]
  type nonrec t = (Arith_expr.Labelled.meta, meta) t

  let compare x = compare Arith_expr.Labelled.compare_meta compare_meta x
  let sexp_of_t x = sexp_of_t Arith_expr.Labelled.sexp_of_meta sexp_of_meta x
  let t_of_sexp x = t_of_sexp Arith_expr.Labelled.meta_of_sexp meta_of_sexp x

  let hash_fold_t =
    hash_fold_t Arith_expr.Labelled.hash_fold_meta hash_fold_meta
  ;;

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let label_of { meta; _ } = meta.label

  let label bool_expr : t =
    let incr_label =
      State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
    in
    let f _ =
      incr_label |> State.map ~f:(fun label -> { Arith_expr.Labelled.label })
    and g _ = incr_label |> State.map ~f:(fun label -> { label }) in
    bitraverse_state ~f ~g bool_expr |> State.run_state ~init:0 |> fst
  ;;
end
