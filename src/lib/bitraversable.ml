open Core_kernel

module type S = sig
  module F : Bifoldable_bifunctor.S
  module M : Monad.S

  val bitraverse
    :  f:('a -> 'c M.t)
    -> g:('b -> 'd M.t)
    -> ('a, 'b) F.t
    -> ('c, 'd) F.t M.t
end

module type S2 = sig
  module F : Bifoldable_bifunctor.S
  module M : Monad.S2

  val bitraverse
    :  f:('a -> ('c, 'e) M.t)
    -> g:('b -> ('d, 'e) M.t)
    -> ('a, 'b) F.t
    -> (('c, 'd) F.t, 'e) M.t
end
