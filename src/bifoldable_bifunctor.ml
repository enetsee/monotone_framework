module type S = sig
  include Bifunctor.S
  include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t
end
