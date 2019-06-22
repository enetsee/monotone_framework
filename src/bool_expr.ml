open Core_kernel

type bool_op =
  | And
  | Or
[@@deriving compare, sexp]

type rel_op =
  | Eq
  | Gt
  | Lt
[@@deriving compare, sexp]

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
    [@@deriving map, fold, compare, sexp]

    let bimap ~f ~g x = map f g x
    let bifold_left ~f ~g ~init x = fold f g init x
    let bifold_right = `Define_using_bifold_left
  end

  module F = struct
    include Basic
    include Bifunctor.Make (Basic)
    include Bifoldable.Make (Basic)
  end

  include F

  module Make_bitraversable (X : Monad.S) :
    Bitraversable.S with module F := F and module M := X = struct
    let bitraverse ~f ~g = function
      | True -> X.return @@ True
      | False -> X.return @@ False
      | Not e -> X.map ~f:(fun e -> Not e) (g e)
      | Boolop (e1, op, e2) ->
        X.(
          g e1
          >>= fun e1' -> g e2 >>= fun e2' -> return @@ Boolop (e1', op, e2'))
      | Relop (e1, op, e2) ->
        X.(
          f e1 >>= fun e1' -> f e2 >>= fun e2' -> return @@ Relop (e1', op, e2'))
    ;;
  end

  module Make_bitraversable2 (X : Monad.S2) :
    Bitraversable.S2 with module F := F and module M := X = struct
    let bitraverse ~f ~g = function
      | True -> X.return @@ True
      | False -> X.return @@ False
      | Not e -> X.map ~f:(fun e -> Not e) (g e)
      | Boolop (e1, op, e2) ->
        X.(
          g e1
          >>= fun e1' -> g e2 >>= fun e2' -> return @@ Boolop (e1', op, e2'))
      | Relop (e1, op, e2) ->
        X.(
          f e1 >>= fun e1' -> f e2 >>= fun e2' -> return @@ Relop (e1', op, e2'))
    ;;
  end

  let true_ = True
  let false_ = False
  let not_ a = Not a
  let and_ a b = Boolop (a, And, b)
  let or_ a b = Boolop (a, Or, b)
  let eq a b = Relop (a, Eq, b)
  let gt a b = Relop (a, Gt, b)
  let lt a b = Relop (a, Lt, b)
end

module T = Fix.Make2 (Pattern) (Arith_expr)
include T

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

(* open Core_kernel open State.Cps

   type bool_op = | And | Or

   type rel_op = | Eq | Gt | Lt

   module Pattern = struct module T = struct module Basic = struct type ('a,
   'b) t = | True | False | Not of 'b | Boolop of 'b * bool_op * 'b | Relop of
   'a * rel_op * 'a [@@deriving map, fold]

   let bimap ~f ~g x = map f g x let bifold_left ~f ~g ~init x = fold f g init
   x let bifold_right = `Define_using_bifold_left end

   include Basic include Bifunctor.Make (Basic) include Bifoldable.Make (Basic)
   end

   include T

   module Make_bitraversable (X : Monad.S) : Bitraversable.S with module F := T
   and module M := X = struct let bitraverse ~f ~g = function | True ->
   X.return @@ True | False -> X.return @@ False | Not e -> X.map ~f:(fun e ->
   Not e) (g e) | Boolop (e1, op, e2) -> X.( g e1 >>= fun e1' -> g e2 >>= fun
   e2' -> return @@ Boolop (e1', op, e2')) | Relop (e1, op, e2) -> X.( f e1 >>=
   fun e1' -> f e2 >>= fun e2' -> return @@ Relop (e1', op, e2')) ;; end

   module Make_bitraversable_2 (X : Monad.S2) : Bitraversable.S2 with module F
   := T and module M := X = struct let bitraverse ~f ~g = function | True ->
   X.return @@ True | False -> X.return @@ False | Not e -> X.map ~f:(fun e ->
   Not e) (g e) | Boolop (e1, op, e2) -> X.( g e1 >>= fun e1' -> g e2 >>= fun
   e2' -> return @@ Boolop (e1', op, e2')) | Relop (e1, op, e2) -> X.( f e1 >>=
   fun e1' -> f e2 >>= fun e2' -> return @@ Relop (e1', op, e2')) ;; end

   module BitraversableState = Make_bitraversable_2 (State)

   let bitraverse_state = BitraversableState.bitraverse let true_ = True let
   false_ = False let not_ a = Not a let and_ a b = Boolop (a, And, b) let or_
   a b = Boolop (a, Or, b) let eq a b = Relop (a, Eq, b) let gt a b = Relop (a,
   Gt, b) let lt a b = Relop (a, Lt, b) end

   (* == Fixed type
   ============================================================ *)

   module Fixed = struct module Basic = struct type ('a, 'b) t = { pattern :
   ('a Arith_expr.t, ('a, 'b) t) Pattern.t ; meta : 'b }

   let pattern { pattern; _ } = pattern let meta { meta; _ } = meta

   let rec bimap ~f ~g { pattern; meta } = { pattern = Pattern.bimap
   ~f:(Arith_expr.map ~f) ~g:(bimap ~f ~g) pattern ; meta = g meta } ;;

   let discard_meta bool_expr = bimap ~f:(fun _ -> ()) ~g:(fun _ -> ())
   bool_expr ;;

   let discard_meta_pattern bool_expr = Pattern.bimap
   ~f:Arith_expr.discard_meta ~g:discard_meta bool_expr ;;

   let rec bifold_left ~f ~g ~init { pattern; meta } = Pattern.bifold_left
   ~f:(fun accu x -> Arith_expr.fold_left ~f ~init:accu x) ~g:(fun accu x ->
   bifold_left ~f ~g ~init:accu x) ~init:(g init meta) pattern ;;

   let bifold_right = `Define_using_bifold_left

   (* == Bifolds over _pattern_ rather than meta-data ================== *) let
   rec bifold_left_pattern ~f ~g ~init { pattern; _ } = Pattern.bifold_left
   ~f:(fun accu x -> Arith_expr.fold_left_pattern ~f ~init:accu x) ~g:(fun accu
   x -> bifold_left_pattern ~f ~g ~init:accu x) ~init:(g init pattern) pattern
   ;;

   let rec bifold_right_pattern ~f ~g ~init { pattern; _ } =
   Pattern.bifold_right ~f:(fun x accu -> Arith_expr.fold_right_pattern ~f
   ~init:accu x) ~g:(fun x accu -> bifold_right_pattern ~f ~g ~init:accu x)
   ~init pattern |> g pattern ;; end

   include Basic include Bifunctor.Make (Basic) include Bifoldable.Make (Basic)
   end

   include Fixed

   (* == Traversable =========================================================
   *)

   module Make_bitraversable (X : Monad.S) : Bitraversable.S with module F :=
   Fixed and module M := X = struct module BitraversablePattern =
   Pattern.Make_bitraversable (X) module TraversableArithExpr =
   Arith_expr.Make_traversable (X)

   let bitraverse_pattern = BitraversablePattern.bitraverse let
   traverse_arith_expr = TraversableArithExpr.traverse

   let rec bitraverse ~f ~g { pattern; meta } = X.( bitraverse_pattern
   ~f:(traverse_arith_expr ~f) ~g:(bitraverse ~f ~g) pattern >>= fun pattern'
   -> g meta >>= fun meta' -> return { pattern = pattern'; meta = meta' }) ;;
   end

   module Make_bitraversable_2 (X : Monad.S2) : Bitraversable.S2 with module F
   := Fixed and module M := X = struct module BitraversablePattern =
   Pattern.Make_bitraversable_2 (X) module TraversableArithExpr =
   Arith_expr.Make_traversable_2 (X)

   let bitraverse_pattern = BitraversablePattern.bitraverse let
   traverse_arith_expr = TraversableArithExpr.traverse

   let rec bitraverse ~f ~g { pattern; meta } = X.( bitraverse_pattern
   ~f:(traverse_arith_expr ~f) ~g:(bitraverse ~f ~g) pattern >>= fun pattern'
   -> g meta >>= fun meta' -> return { pattern = pattern'; meta = meta' }) ;;
   end

   module BitraversableState = Make_bitraversable_2 (State)

   let bitraverse_state = BitraversableState.bitraverse

   (* == Integer labelled boolean expressions ================================
   *)

   module Labelled = struct type meta = { label : int } type nonrec t =
   (Arith_expr.Labelled.meta, meta) t

   let label_of { meta; _ } = meta.label

   let label bool_expr = let incr_label = State.(get >>= fun label -> put
   (label + 1) >>= fun _ -> return label) in let f _ = incr_label |> State.map
   ~f:(fun label -> { Arith_expr.Labelled.label }) and g _ = incr_label |>
   State.map ~f:(fun label -> { label }) in bitraverse_state ~f ~g bool_expr |>
   State.run_state ~init:0 |> fst ;; end

   (* == Constructors ========================================================
   *)

   let with_meta meta pattern = { pattern; meta } let true_ ~meta = with_meta
   meta Pattern.true_ let true__ = true_ ~meta:() let false_ ~meta = with_meta
   meta Pattern.false_ let false__ = false_ ~meta:() let not_ ~meta a =
   with_meta meta @@ Pattern.not_ a let not__ a = not_ ~meta:() a let and_
   ~meta a b = with_meta meta @@ Pattern.and_ a b let and__ a b = and_ ~meta:()
   a b let or_ ~meta a b = with_meta meta @@ Pattern.or_ a b let or__ a b = or_
   ~meta:() a b let eq ~meta a b = with_meta meta @@ Pattern.eq a b let eq_ a b
   = eq ~meta:() a b let gt ~meta a b = with_meta meta @@ Pattern.gt a b let
   gt_ a b = gt ~meta:() a b let lt ~meta a b = with_meta meta @@ Pattern.lt a
   b let lt_ a b = lt ~meta:() a b *)
