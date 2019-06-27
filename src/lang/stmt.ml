open Core_kernel
open Lib
open State.Cps

module Pattern = struct
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

  module F = struct
    include Basic
    include Trifunctor.Make (Basic)
    include Trifoldable.Make (Basic)
  end

  include F

  let assign name expr = Assign (name, expr)
  let skip = Skip
  let block xs = Block xs
  let if_ test ts fs = If (test, ts, fs)
  let while_ test body = While (test, body)

  module Make_tritraversable (M : Monad.S) :
    Tritraversable.S with module M := M and module F = F = struct
    module F = F

    let tritraverse ~f ~g ~h = function
      | F.Assign (name, a) -> M.(f a >>= fun b -> return @@ assign name b)
      | F.Skip -> M.return F.Skip
      | Block xs -> M.(List.map ~f:h xs |> all |> map ~f:block)
      | If (test, ts, fs) ->
        M.(
          g test
          >>= fun test' -> h ts >>= fun ts' -> h fs |> map ~f:(if_ test' ts'))
      | While (test, body) ->
        M.(g test >>= fun test' -> h body |> map ~f:(while_ test'))
    ;;
  end

  module Make_tritraversable2 (M : Monad.S2) :
    Tritraversable.S2 with module M := M and module F = F = struct
    module F = F

    let tritraverse ~f ~g ~h = function
      | F.Assign (name, a) -> M.(f a >>= fun b -> return @@ assign name b)
      | F.Skip -> M.return F.Skip
      | Block xs -> M.(List.map ~f:h xs |> all |> map ~f:block)
      | If (test, ts, fs) ->
        M.(
          g test
          >>= fun test' -> h ts >>= fun ts' -> h fs |> map ~f:(if_ test' ts'))
      | While (test, body) ->
        M.(g test >>= fun test' -> h body |> map ~f:(while_ test'))
    ;;
  end

  let pp pp_first pp_second pp_third ppf = function
    | Assign (name, a) -> Fmt.pf ppf "%s := %a;" name pp_first a
    | Skip -> Fmt.string ppf "skip;"
    | If (b, tc, fc) ->
      Fmt.pf ppf "if (%a) %a %a" pp_second b pp_third tc pp_third fc
    | While (b, c) -> Fmt.pf ppf "while (%a) %a" pp_second b pp_third c
    | Block cs ->
      Fmt.pf ppf {|{@;<1 2>@[<v>%a@]@;}|} Fmt.(list pp_third ~sep:Fmt.cut) cs
  ;;
end

module Fixed = struct
  module Pattern = Pattern
  include Fix.Make3 (Pattern) (Arith_expr.Fixed) (Bool_expr.Fixed)

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
end

(* == Statements without meta-data ========================================== *)
module Unlabelled = struct
  type meta = unit [@@deriving compare, hash, sexp]

  let pp_meta _ _ = ()

  type nonrec t =
    (Arith_expr.Unlabelled.meta, Bool_expr.Unlabelled.meta, meta) Fixed.t

  let compare x y =
    Fixed.compare
      Arith_expr.Unlabelled.compare_meta
      Bool_expr.Unlabelled.compare_meta
      compare_meta
      x
      y
  ;;

  let sexp_of_t x =
    Fixed.sexp_of_t
      Arith_expr.Unlabelled.sexp_of_meta
      Bool_expr.Unlabelled.sexp_of_meta
      sexp_of_meta
      x
  ;;

  let t_of_sexp x =
    Fixed.t_of_sexp
      Arith_expr.Unlabelled.meta_of_sexp
      Bool_expr.Unlabelled.meta_of_sexp
      meta_of_sexp
      x
  ;;

  let hash_fold_t state x =
    Fixed.hash_fold_t
      Arith_expr.Unlabelled.hash_fold_meta
      Bool_expr.Unlabelled.hash_fold_meta
      hash_fold_meta
      state
      x
  ;;

  let pp =
    Fixed.pp Arith_expr.Unlabelled.pp_meta Bool_expr.Unlabelled.pp_meta pp_meta
  ;;

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let unlabel stmt =
    Fixed.trimap ~f:(fun _ -> ()) ~g:(fun _ -> ()) ~h:(fun _ -> ()) stmt
  ;;
end

(* == Integer labelled statements =========================================== *)

module Labelled = struct
  module Label = Label.Make (struct
    type t = int [@@deriving hash, sexp_of, of_sexp, compare]
  end)

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving compare, hash, sexp]

  let pp_meta ppf { label } = Fmt.pf ppf {|%i : |} label

  type nonrec t =
    (Arith_expr.Labelled.meta, Bool_expr.Labelled.meta, meta) Fixed.t

  let compare x y =
    Fixed.compare
      Arith_expr.Labelled.compare_meta
      Bool_expr.Labelled.compare_meta
      compare_meta
      x
      y
  ;;

  let sexp_of_t x =
    Fixed.sexp_of_t
      Arith_expr.Labelled.sexp_of_meta
      Bool_expr.Labelled.sexp_of_meta
      sexp_of_meta
      x
  ;;

  let t_of_sexp x =
    Fixed.t_of_sexp
      Arith_expr.Labelled.meta_of_sexp
      Bool_expr.Labelled.meta_of_sexp
      meta_of_sexp
      x
  ;;

  let hash_fold_t state x =
    Fixed.hash_fold_t
      Arith_expr.Labelled.hash_fold_meta
      Bool_expr.Labelled.hash_fold_meta
      hash_fold_meta
      state
      x
  ;;

  let pp =
    Fixed.pp Arith_expr.Labelled.pp_meta Bool_expr.Labelled.pp_meta pp_meta
  ;;

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let label_of { Fixed.meta; _ } = meta.label

  module Tritraversable_state = Fixed.Make_tritraversable2 (State)

  let label stmt =
    let incr_label =
      State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
    in
    let f _ =
      State.map ~f:(fun label -> { Arith_expr.Labelled.label }) incr_label
    and g _ =
      State.map ~f:(fun label -> { Bool_expr.Labelled.label }) incr_label
    and h _ = State.map ~f:(fun label -> { label }) incr_label in
    Tritraversable_state.tritraverse ~f ~g ~h stmt
    |> State.run_state ~init:0
    |> fst
  ;;
end
