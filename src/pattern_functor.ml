open Core_kernel

module type S = sig
  type 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  include Functor.S with type 'a t := 'a t
  include Foldable.S with type 'a t := 'a t
  include Sexpable.S1 with type 'a t := 'a t

  module F : sig
    type 'a t

    include Functor.S with type 'a t := 'a t
    include Foldable.S with type 'a t := 'a t
  end

  module Make_traversable (X : Monad.S) :
    Traversable.S with module M := X and module F := F

  module Make_traversable2 (X : Monad.S2) :
    Traversable.S2 with module M := X and module F := F
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

  module F : sig
    type ('a, 'b) t

    include Bifunctor.S with type ('a, 'b) t := ('a, 'b) t
    include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t
  end

  module Make_bitraversable (X : Monad.S) :
    Bitraversable.S with module M := X and module F := F

  module Make_bitraversable2 (X : Monad.S2) :
    Bitraversable.S2 with module M := X and module F := F
end

module type F3 = sig
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

module type S3 = sig
  module F : F3
  include F3

  module Make_tritraversable (X : Monad.S) :
    Tritraversable.S with module M := X and module F := F

  module Make_tritraversable2 (X : Monad.S2) :
    Tritraversable.S2 with module M := X and module F := F
end
