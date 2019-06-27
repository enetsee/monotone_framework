open Core_kernel
open Lib
open State.Cps

type op =
  | Plus
  | Minus
  | Mult
  | Div
[@@deriving compare, sexp, hash]

let pp_op ppf = function
  | Plus -> Fmt.string ppf "+"
  | Minus -> Fmt.string ppf "-"
  | Mult -> Fmt.string ppf "*"
  | Div -> Fmt.string ppf "/"
;;

module Pattern = struct
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

  module F = struct
    include Basic
    include Foldable.Make (Basic)
  end

  module Make_traversable (M : Monad.S) :
    Traversable.S with module M := M and module F = F = struct
    module F = F

    let traverse ~f = function
      | F.Lit n -> M.return @@ F.Lit n
      | Var n -> M.return @@ F.Var n
      | Binop (e1, op, e2) ->
        M.(
          f e1
          >>= fun e1' -> f e2 >>= fun e2' -> return @@ F.Binop (e1', op, e2'))
    ;;
  end

  module Make_traversable2 (M : Monad.S2) :
    Traversable.S2 with module M := M and module F = F = struct
    module F = F

    let traverse ~f = function
      | F.Lit n -> M.return @@ F.Lit n
      | Var n -> M.return @@ F.Var n
      | Binop (e1, op, e2) ->
        M.(
          f e1
          >>= fun e1' -> f e2 >>= fun e2' -> return @@ F.Binop (e1', op, e2'))
    ;;
  end

  include F

  let lit n = Lit n
  let var n = Var n
  let plus a b = Binop (a, Plus, b)
  let minus a b = Binop (a, Minus, b)
  let mult a b = Binop (a, Mult, b)
  let div a b = Binop (a, Div, b)

  let pp pp_with ppf = function
    | Lit n -> Fmt.pf ppf {|%i|} n
    | Var n -> Fmt.pf ppf {|%s|} n
    | Binop (e1, op, e2) ->
      Fmt.pf ppf {|%a %a %a|} pp_with e1 pp_op op pp_with e2
  ;;
end

module Fixed = struct
  module Pattern = Pattern
  include Fix.Make (Pattern)

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
end

(* == Arithmetic expressions with no meta-data ============================== *)

module Unlabelled = struct
  type meta = unit [@@deriving sexp, hash, compare]
  type nonrec t = meta Fixed.t

  let pp_meta _ _ = ()
  let compare x = Fixed.compare compare_meta x
  let sexp_of_t x = Fixed.sexp_of_t sexp_of_meta x
  let t_of_sexp x = Fixed.t_of_sexp meta_of_sexp x
  let hash_fold_t = Fixed.hash_fold_t hash_fold_meta
  let pp ppf x = Fixed.pp pp_meta ppf x

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let unlabel arith_expr = Fixed.map ~f:(fun _ -> ()) arith_expr
end

(* == Integer labelled arithmetic expressions =============================== *)

module Labelled = struct
  module Label = Label.Make (struct
    type t = int [@@deriving hash, sexp_of, of_sexp, compare]
  end)

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving sexp, hash, compare]

  let pp_meta ppf { label } = Fmt.pf ppf {|%i : |} label

  type nonrec t = meta Fixed.t

  let compare x y = Fixed.compare compare_meta x y
  let sexp_of_t x = Fixed.sexp_of_t sexp_of_meta x
  let t_of_sexp x = Fixed.t_of_sexp meta_of_sexp x
  let hash_fold_t = Fixed.hash_fold_t hash_fold_meta
  let pp ppf x = Fixed.pp pp_meta ppf x

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)

  let label_of { Fixed.meta; _ } = meta.label

  module Traversable_state = Fixed.Make_traversable2 (State)

  let label arith_expr =
    let f _ =
      State.(
        get >>= fun label -> put (label + 1) >>= fun _ -> return { label })
    in
    Traversable_state.traverse ~f arith_expr |> State.run_state ~init:0 |> fst
  ;;
end
