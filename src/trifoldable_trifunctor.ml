module type S = sig
  include Trifunctor.S
  include Trifoldable.S with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end
