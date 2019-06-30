open Core_kernel
open Lib

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
  val is_trivial : 'a t -> bool
end

module Fixed : sig
  include Fix.S with module Pattern = Pattern

  val lit : 'a -> int -> 'a t
  val lit_ : int -> unit t
  val var : 'a -> string -> 'a t
  val var_ : string -> unit t
  val plus : 'a -> 'a t -> 'a t -> 'a t
  val plus_ : unit t -> unit t -> unit t
  val minus : 'a -> 'a t -> 'a t -> 'a t
  val minus_ : unit t -> unit t -> unit t
  val mult : 'a -> 'a t -> 'a t -> 'a t
  val mult_ : unit t -> unit t -> unit t
  val div : 'a -> 'a t -> 'a t -> 'a t
  val div_ : unit t -> unit t -> unit t
  val pp_ : Format.formatter -> 'a t -> unit
  val is_trivial : 'a t -> bool
  val free_vars : ?init:string list -> 'a t -> string list
  val eval : ?env:'a t StringMap.t -> ?cont:('a t -> 'a t) -> 'a t -> 'a t
end

module Unlabelled : sig
  type meta = unit [@@deriving compare, sexp, hash]
  type nonrec t = meta Fixed.t

  val pp_meta : Format.formatter -> meta -> unit

  include Sexpable.S with type t := t
  include Comparator.S with type t := t
  include Pretty.S with type t := t

  val hash_fold_t : Hash.state -> t -> Hash.state
  val unlabel : 'a Fixed.t -> t
end

module Labelled : sig
  module Label : Lib.Label.S with type t = int
  module LabelMap : Map.S with module Key := Label

  type meta = { label : Label.t [@compare.ignore] }
  [@@deriving compare, sexp, hash]

  val pp_meta : Format.formatter -> meta -> unit

  type nonrec t = meta Fixed.t

  include Sexpable.S with type t := t
  include Comparator.S with type t := t
  include Pretty.S with type t := t

  val hash_fold_t : Hash.state -> t -> Hash.state
  val label : 'a Fixed.t -> t
  val label_of : t -> Label.t

  type associations = t LabelMap.t

  val associate : ?init:associations -> t -> associations
end
