open Core_kernel
open Lib
open State.Cps

type op =
  | Plus
  | Minus
  | Mult
  | Div
[@@deriving compare, sexp, hash]

module Pattern : sig
  type 'a t =
    | Lit of int
    | Var of string
    | Binop of 'a * op * 'a

  include Pattern_functor.S with type 'a t := 'a t

  val lit : int -> 'a t
  val var : string -> 'a t
  val plus : 'a -> 'a -> 'a t
  val minus : 'a -> 'a -> 'a t
  val mult : 'a -> 'a -> 'a t
  val div : 'a -> 'a -> 'a t
end = struct
  module Basic = struct
    type 'a t =
      | Lit of int
      | Var of string
      | Binop of 'a * op * 'a
    [@@deriving map, fold, compare, sexp, hash]

    let map ~f x = map f x
    let fold_left ~f ~init x = fold f init x
    let fold_right = `Define_using_fold_left
  end

  include Basic
  include Foldable.Make (Basic)

  let lit n = Lit n
  let var n = Var n
  let plus a b = Binop (a, Plus, b)
  let minus a b = Binop (a, Minus, b)
  let mult a b = Binop (a, Mult, b)
  let div a b = Binop (a, Div, b)
end

module Fixed = Fix.Make (Pattern)
include Fixed

(* == Constructors ========================================================== *)

let lit meta n = fix meta @@ Pattern.lit n
let lit_ n = lit () n
let var meta n = fix meta @@ Pattern.var n
let var_ n = var () n
let plus meta a b = fix meta @@ Pattern.plus a b
let plus_ a b = plus () a b
let minus meta a b = fix meta @@ Pattern.minus a b
let minus_ a b = minus () a b
let mult meta a b = fix meta @@ Pattern.mult a b
let mult_ a b = mult () a b
let div meta a b = fix meta @@ Pattern.div a b
let div_ a b = div () a b

(* == Traversable Functors ================================================== *)

module Make_traversable_pattern (M : Monad.S) :
  Traversable.S with module M := M and module F := Pattern = struct
  let traverse ~f = function
    | Pattern.Lit n -> M.return @@ Pattern.Lit n
    | Var n -> M.return @@ Pattern.Var n
    | Binop (e1, op, e2) ->
      M.(
        f e1
        >>= fun e1' ->
        f e2 >>= fun e2' -> return @@ Pattern.Binop (e1', op, e2'))
  ;;
end

module Make_traversable_pattern2 (M : Monad.S2) :
  Traversable.S2 with module M := M and module F := Pattern = struct
  let traverse ~f = function
    | Pattern.Lit n -> M.return @@ Pattern.Lit n
    | Var n -> M.return @@ Pattern.Var n
    | Binop (e1, op, e2) ->
      M.(
        f e1
        >>= fun e1' ->
        f e2 >>= fun e2' -> return @@ Pattern.Binop (e1', op, e2'))
  ;;
end

module Make_traversable (M : Monad.S) :
  Traversable.S with module M := M and module F := Fixed = struct
  module TraversablePattern = Make_traversable_pattern (M)

  let traverse_pattern = TraversablePattern.traverse

  let rec traverse ~f { pattern; meta } =
    M.(
      f meta
      >>= fun meta' ->
      traverse_pattern ~f:(traverse ~f) pattern |> map ~f:(fix meta'))
  ;;
end

module Make_traversable2 (M : Monad.S2) :
  Traversable.S2 with module M := M and module F := Fixed = struct
  module TraversablePattern = Make_traversable_pattern2 (M)

  let traverse_pattern = TraversablePattern.traverse

  let rec traverse ~f { pattern; meta } =
    M.(
      f meta
      >>= fun meta' ->
      traverse_pattern ~f:(traverse ~f) pattern |> map ~f:(fix meta'))
  ;;
end

module Traversable_state = Make_traversable2 (State)

let traverse_state = Traversable_state.traverse

(* == Arithmetic expressions with no meta-data ============================== *)

module Unlabelled = struct
  type meta = unit [@@deriving sexp, hash, compare]
  type nonrec t = meta t

  let compare x = compare compare_meta x
  let sexp_of_t x = sexp_of_t sexp_of_meta x
  let t_of_sexp x = t_of_sexp meta_of_sexp x
  let hash_fold_t = hash_fold_t hash_fold_meta

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let unlabel arith_expr = map ~f:(fun _ -> ()) arith_expr
end

(* == Integer labelled arithmetic expressions =============================== *)

module Labelled = struct

  module Label = Label.Make (struct
    type t = int [@@deriving hash, sexp_of, of_sexp, compare]
  end)  

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving sexp, hash, compare]

  type nonrec t = meta t

  let compare (x : t) (y : t) = compare compare_meta x y
  let sexp_of_t x = sexp_of_t sexp_of_meta x
  let t_of_sexp x = t_of_sexp meta_of_sexp x
  let hash_fold_t = hash_fold_t hash_fold_meta

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let label_of { meta; _ } = meta.label

  let label arith_expr =
    let f _ =
      State.(
        get >>= fun label -> put (label + 1) >>= fun _ -> return { label })
    in
    traverse_state ~f arith_expr |> State.run_state ~init:0 |> fst
  ;;
end
