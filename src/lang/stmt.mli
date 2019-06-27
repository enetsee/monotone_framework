open Core_kernel
open Lib

module Pattern : sig
  type ('a, 'b, 'c) t =
    | Assign of string * 'a
    | Skip
    | Block of 'c list
    | If of 'b * 'c * 'c
    | While of 'b * 'c

  val assign : string -> 'a -> ('a, 'b, 'c) t
  val skip : ('a, 'b, 'c) t
  val block : 'c list -> ('a, 'b, 'c) t
  val if_ : 'b -> 'c -> 'c -> ('a, 'b, 'c) t
  val while_ : 'b -> 'c -> ('a, 'b, 'c) t

  include Pattern_functor.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module Fixed : sig
  include
    Fix.S3
    with module Pattern = Pattern
     and module First := Arith_expr.Fixed
     and module Second := Bool_expr.Fixed

  val assign : 'c -> string -> 'a Arith_expr.Fixed.t -> ('a, 'b, 'c) t
  val skip : 'c -> ('a, 'b, 'c) t
  val block : 'c -> ('a, 'b, 'c) t list -> ('a, 'b, 'c) t

  val if_
    :  'c
    -> ('a, 'b) Bool_expr.Fixed.t
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t

  val while_
    :  'c
    -> ('a, 'b) Bool_expr.Fixed.t
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t

  val assign_ : string -> unit Arith_expr.Fixed.t -> (unit, unit, unit) t
  val skip_ : (unit, unit, unit) t
  val block_ : (unit, unit, unit) t list -> (unit, unit, unit) t

  val if__
    :  (unit, unit) Bool_expr.Fixed.t
    -> (unit, unit, unit) t
    -> (unit, unit, unit) t
    -> (unit, unit, unit) t

  val while__
    :  (unit, unit) Bool_expr.Fixed.t
    -> (unit, unit, unit) t
    -> (unit, unit, unit) t
end

module Unlabelled : sig
  type meta = unit [@@deriving compare, hash, sexp]

  val pp_meta : Format.formatter -> meta -> unit

  type nonrec t =
    (Arith_expr.Unlabelled.meta, Bool_expr.Unlabelled.meta, meta) Fixed.t

  include Sexpable.S with type t := t
  include Comparator.S with type t := t
  include Pretty.S with type t := t

  val hash_fold_t : Hash.state -> t -> Hash.state
  val unlabel : ('a, 'b, 'c) Fixed.t -> t
end

module Labelled : sig
  module Label : Lib.Label.S with type t = int

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving compare, hash, sexp]

  val pp_meta : Format.formatter -> meta -> unit

  type nonrec t =
    (Arith_expr.Labelled.meta, Bool_expr.Labelled.meta, meta) Fixed.t

  include Sexpable.S with type t := t
  include Comparator.S with type t := t
  include Pretty.S with type t := t

  val hash_fold_t : Hash.state -> t -> Hash.state
  val label : ('a, 'b, 'c) Fixed.t -> t
  val label_of : t -> Label.t
end
