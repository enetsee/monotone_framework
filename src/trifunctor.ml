module type Basic = sig
  type ('a, 'b, 'c) t

  val trimap
    :  f:('a -> 'd)
    -> g:('b -> 'e)
    -> h:('c -> 'f)
    -> ('a, 'b, 'c) t
    -> ('d, 'e, 'f) t
end

module type S = sig
  include Basic

  val map_first : f:('a -> 'b) -> ('a, 'c, 'd) t -> ('b, 'c, 'd) t
  val map_second : f:('b -> 'c) -> ('a, 'b, 'd) t -> ('a, 'c, 'd) t
  val map_third : f:('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
end

module Make (X : Basic) : S with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t =
struct
  let trimap = X.trimap
  let map_first ~f x = trimap ~f ~g:(fun x -> x) ~h:(fun x -> x) x
  let map_second ~f x = trimap ~f:(fun x -> x) ~g:f ~h:(fun x -> x) x
  let map_third ~f x = trimap ~f:(fun x -> x) ~g:(fun x -> x) ~h:f x
end
