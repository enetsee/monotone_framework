module type S = sig
  type 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module type S2 = sig
  type ('a, 'b) t

  val map : f:('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
end
