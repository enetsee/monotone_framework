open Core_kernel
open Lib
open State.Cps

module Pattern : sig
  type ('a, 'b, 's) t =
    | Assign of string * 'a
    | Skip
    | Block of 's list
    | If of 'b * 's * 's
    | While of 'b * 's

  include Pattern_functor.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  val assign : string -> 'a -> ('a, 'b, 'c) t
  val skip : ('a, 'b, 'c) t
  val block : 'c list -> ('a, 'b, 'c) t
  val if_ : 'b -> 'c -> 'c -> ('a, 'b, 'c) t
  val while_ : 'b -> 'c -> ('a, 'b, 'c) t
end = struct
  module Basic = struct
    type ('a, 'b, 's) t =
      | Assign of string * 'a
      | Skip
      | Block of 's list
      | If of 'b * 's * 's
      | While of 'b * 's
    [@@deriving map, fold, sexp, compare, hash]

    let trimap ~f ~g ~h x = map f g h x
    let trifold_left ~f ~g ~h ~init x = fold f g h init x
    let trifold_right = `Define_using_trifold_left
  end

  include Basic
  include Trifunctor.Make (Basic)
  include Trifoldable.Make (Basic)

  let assign name expr = Assign (name, expr)
  let skip = Skip
  let block xs = Block xs
  let if_ test ts fs = If (test, ts, fs)
  let while_ test body = While (test, body)
end

module Fixed = Fix.Make3 (Pattern) (Arith_expr) (Bool_expr)
include Fixed

(* == Constructors ========================================================== *)
let assign meta name expr = fix meta @@ Pattern.assign name expr
let assign_ name expr = assign () name expr
let skip meta = fix meta @@ Pattern.skip
let skip_ : (unit, unit, unit) t = skip ()
let block meta xs = fix meta @@ Pattern.block xs
let block_ xs = block () xs
let if_ meta test ts fs = fix meta @@ Pattern.if_ test ts fs
let if__ test ts fs = if_ () test ts fs
let while_ meta test body = fix meta @@ Pattern.while_ test body
let while__ test body = while_ () test body

(* == Traversable Functors ================================================== *)

module Make_tritraversable_pattern (M : Monad.S) :
  Tritraversable.S with module M := M and module F := Pattern = struct
  let tritraverse ~f ~g ~h = function
    | Pattern.Assign (name, a) -> M.map ~f:(Pattern.assign name) (f a)
    | Skip -> M.return Pattern.Skip
    | Block xs -> M.(List.map ~f:h xs |> all |> map ~f:Pattern.block)
    | If (test, ts, fs) ->
      M.(
        g test
        >>= fun test' ->
        h ts >>= fun ts' -> h fs |> map ~f:(Pattern.if_ test' ts'))
    | While (test, body) ->
      M.(g test >>= fun test' -> h body |> map ~f:(Pattern.while_ test'))
  ;;
end

module Make_tritraversable_pattern2 (M : Monad.S2) :
  Tritraversable.S2 with module M := M and module F := Pattern = struct
  let tritraverse ~f ~g ~h = function
    | Pattern.Assign (name, a) -> M.map ~f:(Pattern.assign name) (f a)
    | Skip -> M.return Pattern.Skip
    | Block xs -> M.(List.map ~f:h xs |> all |> map ~f:Pattern.block)
    | If (test, ts, fs) ->
      M.(
        g test
        >>= fun test' ->
        h ts >>= fun ts' -> h fs |> map ~f:(Pattern.if_ test' ts'))
    | While (test, body) ->
      M.(g test >>= fun test' -> h body |> map ~f:(Pattern.while_ test'))
  ;;
end

module Make_tritraversable (M : Monad.S) :
  Tritraversable.S with module M := M and module F := Fixed = struct
  module TritraversablePattern = Make_tritraversable_pattern (M)
  module BitraversableBoolExpr = Bool_expr.Make_bitraversable (M)
  module TraversableArithExpr = Arith_expr.Make_traversable (M)

  let tritraverse_pattern = TritraversablePattern.tritraverse
  let bitraverse_bool_expr = BitraversableBoolExpr.bitraverse
  let traverse_arith_expr = TraversableArithExpr.traverse

  let rec tritraverse ~f ~g ~h { pattern; meta } =
    M.(
      h meta
      >>= fun meta' ->
      tritraverse_pattern
        ~f:(traverse_arith_expr ~f)
        ~g:(bitraverse_bool_expr ~f ~g)
        ~h:(tritraverse ~f ~g ~h)
        pattern
      |> map ~f:(fix meta'))
  ;;
end

module Make_tritraversable2 (M : Monad.S2) :
  Tritraversable.S2 with module M := M and module F := Fixed = struct
  module TritraversablePattern = Make_tritraversable_pattern2 (M)
  module BitraversableBoolExpr = Bool_expr.Make_bitraversable2 (M)
  module TraversableArithExpr = Arith_expr.Make_traversable2 (M)

  let tritraverse_pattern = TritraversablePattern.tritraverse
  let bitraverse_bool_expr = BitraversableBoolExpr.bitraverse
  let traverse_arith_expr = TraversableArithExpr.traverse

  let rec tritraverse ~f ~g ~h { pattern; meta } =
    M.(
      h meta
      >>= fun meta' ->
      tritraverse_pattern
        ~f:(traverse_arith_expr ~f)
        ~g:(bitraverse_bool_expr ~f ~g)
        ~h:(tritraverse ~f ~g ~h)
        pattern
      |> map ~f:(fix meta'))
  ;;
end

module Tritraversable_state = Make_tritraversable2 (State)

let tritraverse_state = Tritraversable_state.tritraverse

(* == Statements without meta-data ========================================== *)

module Unlabelled = struct
  type meta = unit [@@deriving compare, hash, sexp]

  type nonrec t =
    (Arith_expr.Unlabelled.meta, Bool_expr.Unlabelled.meta, meta) t

  let compare (x : t) (y : t) =
    compare
      Arith_expr.Unlabelled.compare_meta
      Bool_expr.Unlabelled.compare_meta
      compare_meta
      x
      y
  ;;

  let sexp_of_t (x : t) =
    sexp_of_t
      Arith_expr.Unlabelled.sexp_of_meta
      Bool_expr.Unlabelled.sexp_of_meta
      sexp_of_meta
      x
  ;;

  let t_of_sexp x : t =
    t_of_sexp
      Arith_expr.Unlabelled.meta_of_sexp
      Bool_expr.Unlabelled.meta_of_sexp
      meta_of_sexp
      x
  ;;

  let hash_fold_t state (x : t) =
    hash_fold_t
      Arith_expr.Unlabelled.hash_fold_meta
      Bool_expr.Unlabelled.hash_fold_meta
      hash_fold_meta
      state
      x
  ;;

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let unlabel stmt : t =
    trimap ~f:(fun _ -> ()) ~g:(fun _ -> ()) ~h:(fun _ -> ()) stmt
  ;;
end

(* == Integer labelled statements =========================================== *)

module Labelled = struct

  module Label = Label.Make (struct
    type t = int [@@deriving hash, sexp_of, of_sexp, compare]
  end)  

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving compare, hash, sexp]

  type nonrec t = (Arith_expr.Labelled.meta, Bool_expr.Labelled.meta, meta) t

  let compare (x : t) (y : t) =
    compare
      Arith_expr.Labelled.compare_meta
      Bool_expr.Labelled.compare_meta
      compare_meta
      x
      y
  ;;

  let sexp_of_t (x : t) =
    sexp_of_t
      Arith_expr.Labelled.sexp_of_meta
      Bool_expr.Labelled.sexp_of_meta
      sexp_of_meta
      x
  ;;

  let t_of_sexp x : t =
    t_of_sexp
      Arith_expr.Labelled.meta_of_sexp
      Bool_expr.Labelled.meta_of_sexp
      meta_of_sexp
      x
  ;;

  let hash_fold_t state (x : t) =
    hash_fold_t
      Arith_expr.Labelled.hash_fold_meta
      Bool_expr.Labelled.hash_fold_meta
      hash_fold_meta
      state
      x
  ;;

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let label_of { meta; _ } = meta.label

  let label stmt : t =
    let incr_label =
      State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
    in
    let f _ =
      State.map ~f:(fun label -> { Arith_expr.Labelled.label }) incr_label
    and g _ =
      State.map ~f:(fun label -> { Bool_expr.Labelled.label }) incr_label
    and h _ = State.map ~f:(fun label -> { label }) incr_label in
    tritraverse_state ~f ~g ~h stmt |> State.run_state ~init:0 |> fst
  ;;
end
