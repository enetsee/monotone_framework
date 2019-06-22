open Core_kernel

module type S = sig
  module F : Trifoldable_trifunctor.S
  module M : Monad.S

  val tritraverse
    :  f:('a -> 'd M.t)
    -> g:('b -> 'e M.t)
    -> h:('c -> 'f M.t)
    -> ('a, 'b, 'c) F.t
    -> ('d, 'e, 'f) F.t M.t
end

module type S2 = sig
  module F : Trifoldable_trifunctor.S
  module M : Monad.S2

  val tritraverse
    :  f:('a -> ('d, 'g) M.t)
    -> g:('b -> ('e, 'g) M.t)
    -> h:('c -> ('f, 'g) M.t)
    -> ('a, 'b, 'c) F.t
    -> (('d, 'e, 'f) F.t, 'g) M.t
end
