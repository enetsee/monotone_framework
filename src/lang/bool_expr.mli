open Core_kernel
open Lib

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
end

module Fixed : sig
  include
    Fix.S2 with module Pattern = Pattern and module First := Arith_expr.Fixed

  val true_ : 'b -> ('a, 'b) t
  val false_ : 'b -> ('a, 'b) t
  val not_ : 'b -> ('a, 'b) t -> ('a, 'b) t
  val and_ : 'b -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val or_ : 'b -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val eq : 'b -> 'a Arith_expr.Fixed.t -> 'a Arith_expr.Fixed.t -> ('a, 'b) t
  val gt : 'b -> 'a Arith_expr.Fixed.t -> 'a Arith_expr.Fixed.t -> ('a, 'b) t
  val lt : 'b -> 'a Arith_expr.Fixed.t -> 'a Arith_expr.Fixed.t -> ('a, 'b) t
  val true__ : (unit, unit) t
  val false__ : (unit, unit) t
  val not__ : (unit, unit) t -> (unit, unit) t
  val and__ : (unit, unit) t -> (unit, unit) t -> (unit, unit) t
  val or__ : (unit, unit) t -> (unit, unit) t -> (unit, unit) t

  val eq_
    :  unit Arith_expr.Fixed.t
    -> unit Arith_expr.Fixed.t
    -> (unit, unit) t

  val gt_
    :  unit Arith_expr.Fixed.t
    -> unit Arith_expr.Fixed.t
    -> (unit, unit) t

  val lt_
    :  unit Arith_expr.Fixed.t
    -> unit Arith_expr.Fixed.t
    -> (unit, unit) t
end

module Unlabelled : sig
  type meta = unit [@@deriving compare, sexp, hash]
  type nonrec t = (Arith_expr.Unlabelled.meta, meta) Fixed.t

  val pp_meta : Format.formatter -> meta -> unit

  include Sexpable.S with type t := t
  include Comparator.S with type t := t
  include Pretty.S with type t := t

  val hash_fold_t : Hash.state -> t -> Hash.state
  val unlabel : ('a, 'b) Fixed.t -> t
end

module Labelled : sig
  module Label : Lib.Label.S with type t = int

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving compare, hash, sexp]

  val pp_meta : Format.formatter -> meta -> unit

  type nonrec t = (Arith_expr.Labelled.meta, meta) Fixed.t

  include Sexpable.S with type t := t
  include Comparator.S with type t := t
  include Pretty.S with type t := t

  val hash_fold_t : Hash.state -> t -> Hash.state
  val label : ('a, 'b) Fixed.t -> t
  val label_of : t -> Label.t
end
