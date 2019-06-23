module type S = sig
  include Functor.S
  include Foldable.S with type 'a t := 'a t
end
