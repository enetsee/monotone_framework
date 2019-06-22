open Core_kernel

type op =
  | Plus
  | Minus
  | Mult
  | Div
[@@deriving compare, sexp]

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
    [@@deriving map, fold, compare, sexp]

    let map ~f x = map f x
    let fold_left ~f ~init x = fold f init x
    let fold_right = `Define_using_fold_left
  end

  module F = struct
    include Basic
    include Foldable.Make (Basic)
  end

  include F

  module Make_traversable (X : Monad.S) :
    Traversable.S with module M := X and module F := F = struct
    let traverse ~f = function
      | Lit n -> X.return @@ Lit n
      | Var n -> X.return @@ Var n
      | Binop (e1, op, e2) ->
        X.(
          f e1 >>= fun e1' -> f e2 >>= fun e2' -> return @@ Binop (e1', op, e2'))
    ;;
  end

  module Make_traversable2 (X : Monad.S2) :
    Traversable.S2 with module M := X and module F := F = struct
    let traverse ~f = function
      | Lit n -> X.return @@ Lit n
      | Var n -> X.return @@ Var n
      | Binop (e1, op, e2) ->
        X.(
          f e1 >>= fun e1' -> f e2 >>= fun e2' -> return @@ Binop (e1', op, e2'))
    ;;
  end

  let lit n = Lit n
  let var n = Var n
  let plus a b = Binop (a, Plus, b)
  let minus a b = Binop (a, Minus, b)
  let mult a b = Binop (a, Mult, b)
  let div a b = Binop (a, Div, b)
end

module Fixed = Fix.Make (Pattern)
include Fixed

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

(* 

   module Pattern = struct

   module T = struct module Basic = struct type 'a t = | Lit of int | Var of
   string | Binop of 'a * op * 'a [@@deriving map, fold]

   let map ~f x = map f x

   let fold_left ~f ~init x = fold f init x

   let fold_right = `Define_using_fold_left end include Basic include
   Foldable.Make(Basic) end

   include T

   module Make_traversable(X: Monad.S) : Traversable.S with module F := T and
   module M := X = struct let traverse ~f = function | Lit n -> X.return @@ Lit
   n | Var n -> X.return @@ Var n | Binop(e1,op,e2) -> X.( f e1 >>= fun e1' ->
   f e2 >>= fun e2' -> return @@ Binop(e1',op,e2') ) end

   module Make_traversable_2(X: Monad.S2) : Traversable.S2 with module F := T
   and module M := X = struct let traverse ~f = function | Lit n -> X.return @@
   Lit n | Var n -> X.return @@ Var n | Binop(e1,op,e2) -> X.( f e1 >>= fun e1'
   -> f e2 >>= fun e2' -> return @@ Binop(e1',op,e2') ) end

   module Traversable_state = Make_traversable_2(State)

   let traverse_state = Traversable_state.traverse

   let lit n = Lit n

   let var n = Var n

   let plus a b = Binop(a,Plus,b)

   let minus a b = Binop(a,Minus,b)

   let mult a b = Binop(a,Mult,b)

   let div a b = Binop(a,Div,b)

   end

   module T = struct module Basic = struct (** Fixed arithmetic expressions
   with meta data *) type 'a t = { pattern : 'a t Pattern.t ; meta : 'a}

   let pattern {pattern;_} = pattern

   let meta {meta;_} = meta

   let rec map ~f { pattern ; meta } = { pattern = Pattern.map ~f:(map ~f)
   pattern ; meta = f meta }

   let discard_meta arith_expr = map ~f:(fun _ -> ()) arith_expr

   let discard_meta_pattern arith_expr = Pattern.map ~f:discard_meta arith_expr

   let rec fold_left ~f ~init {pattern ; meta } = Pattern.fold_left ~f:(fun
   accu x -> fold_left ~f ~init:accu x) ~init:(f init meta) pattern

   let fold_right = `Define_using_fold_left

   (* == Folds over _pattern_ rather than meta-data ====================== *)
   let rec fold_left_pattern ~f ~init {pattern;_} = Pattern.fold_left ~f:(fun
   accu x -> fold_left_pattern ~f ~init:accu x) ~init:(f init pattern) pattern

   let rec fold_right_pattern ~f ~init {pattern;_} = Pattern.fold_right ~f:(fun
   x accu -> fold_right_pattern ~f ~init:accu x) ~init pattern |> f pattern

   end

   include Basic

   include Foldable.Make(Basic) end

   include T

   (* == Traversable =========================================================
   *)

   module Make_traversable(X: Monad.S) : Traversable.S with module F := T and
   module M := X = struct

   module TraversablePattern = Pattern.Make_traversable(X)

   let traverse_pattern = TraversablePattern.traverse

   let rec traverse ~f {pattern;meta} = X.( traverse_pattern ~f:(traverse ~f)
   pattern >>= fun pattern' -> f meta >>= fun meta' -> return { pattern =
   pattern'; meta = meta'} ) end

   module Make_traversable_2(X: Monad.S2) : Traversable.S2 with module F := T
   and module M := X = struct

   module TraversablePattern = Pattern.Make_traversable_2(X)

   let traverse_pattern = TraversablePattern.traverse

   let rec traverse ~f {pattern;meta} = X.( traverse_pattern ~f:(traverse ~f)
   pattern >>= fun pattern' -> f meta >>= fun meta' -> return { pattern =
   pattern'; meta = meta'} ) end

   module TraversableState = Make_traversable_2(State)

   let traverse_state = TraversableState.traverse

   (* == Integer labelled arithmetic expressions =============================
   *)

   module Labelled = struct type meta = { label : int } type nonrec t = meta t

   let label_of {meta;_} = meta.label

   let label arith_expr = let f _ = State.( get >>= fun label -> put (label +
   1) >>= fun _ -> return { label } ) in traverse_state ~f arith_expr |>
   State.run_state ~init:0 |> fst end

   (* == Constructors ========================================================
   *)

   let with_meta meta pattern = {pattern; meta}

   let lit ~meta n = with_meta meta @@ Pattern.lit n let lit_ n = lit ~meta:()
   n

   let var ~meta n = with_meta meta @@ Pattern.var n let var_ n = var ~meta:()
   n

   let plus ~meta a b = with_meta meta @@ Pattern.plus a b let plus_ a b = plus
   ~meta:() a b

   let minus ~meta a b = with_meta meta @@ Pattern.minus a b let minus_ a b =
   minus ~meta:() a b

   let mult ~meta a b = with_meta meta @@ Pattern.mult a b let mult_ a b = mult
   ~meta:() a b

   let div ~meta a b = with_meta meta @@ Pattern.div a b let div_ a b = div
   ~meta:() a b *)
