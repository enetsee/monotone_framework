open Core_kernel

module type S = sig
  type 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  include Functor.S with type 'a t := 'a t
  include Foldable.S with type 'a t := 'a t
  include Sexpable.S1 with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'b) t

  val compare
    :  ('a -> 'a -> int)
    -> ('b -> 'b -> int)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> int

  include Bifunctor.S with type ('a, 'b) t := ('a, 'b) t
  include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t
  include Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val compare
    :  ('a -> 'a -> int)
    -> ('b -> 'b -> int)
    -> ('c -> 'c -> int)
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
    -> int

  include Trifunctor.S with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Trifoldable.S with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Sexpable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end
