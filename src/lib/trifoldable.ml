open Core_kernel

module type Basic = sig
  type ('a, 'b, 'c) t

  val trifold_left
    :  f:('d -> 'a -> 'd)
    -> g:('d -> 'b -> 'd)
    -> h:('d -> 'c -> 'd)
    -> init:'d
    -> ('a, 'b, 'c) t
    -> 'd

  val trifold_right
    : [ `Custom of
        f:('a -> 'd -> 'd)
        -> g:('b -> 'd -> 'd)
        -> h:('c -> 'd -> 'd)
        -> init:'d
        -> ('a, 'b, 'c) t
        -> 'd
      | `Define_using_trifold_left
      ]
end

module type S = sig
  type ('a, 'b, 'c) t

  val trifold_left
    :  f:('d -> 'a -> 'd)
    -> g:('d -> 'b -> 'd)
    -> h:('d -> 'c -> 'd)
    -> init:'d
    -> ('a, 'b, 'c) t
    -> 'd

  val trifold_right
    :  f:('a -> 'd -> 'd)
    -> g:('b -> 'd -> 'd)
    -> h:('c -> 'd -> 'd)
    -> init:'d
    -> ('a, 'b, 'c) t
    -> 'd

  val trifold_map
    :  (module Monoid.S with type t = 'a)
    -> f:('b -> 'a)
    -> g:('c -> 'a)
    -> h:('d -> 'a)
    -> ?init:'a
    -> ('b, 'c, 'd) t
    -> 'a

  val triany
    :  pred_first:('a -> bool)
    -> pred_second:('b -> bool)
    -> pred_third:('c -> bool)
    -> ?init:bool
    -> ('a, 'b, 'c) t
    -> bool

  val triall
    :  pred_first:('a -> bool)
    -> pred_second:('b -> bool)
    -> pred_third:('c -> bool)
    -> ?init:bool
    -> ('a, 'b, 'c) t
    -> bool
end

module Make (X : Basic) : S with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t =
struct
  let trifold_left = X.trifold_left

  let trifold_right_from_trifold_left ~f ~g ~h ~init x =
    let f' k x z = k @@ f x z
    and g' k x z = k @@ g x z
    and h' k x z = k @@ h x z in
    trifold_left ~f:f' ~g:g' ~h:h' ~init:(fun x -> x) x init
  ;;

  let trifold_right =
    match X.trifold_right with
    | `Custom f -> f
    | `Define_using_trifold_left -> trifold_right_from_trifold_left
  ;;

  let trifold_map
      (type a)
      (module M : Monoid.S with type t = a)
      ~f
      ~g
      ~h
      ?init:(empty = M.empty)
      x
    =
    trifold_right
      ~f:(fun x accu -> M.combine accu @@ f x)
      ~g:(fun x accu -> M.combine accu @@ g x)
      ~h:(fun x accu -> M.combine accu @@ h x)
      ~init:empty
      x
  ;;

  let triany ~pred_first ~pred_second ~pred_third ?init x =
    trifold_map
      (module Monoid.Bool_or)
      ~f:pred_first
      ~g:pred_second
      ~h:pred_third
      ?init
      x
  ;;

  let triall ~pred_first ~pred_second ~pred_third ?init x =
    trifold_map
      (module Monoid.Bool_and)
      ~f:pred_first
      ~g:pred_second
      ~h:pred_third
      ?init
      x
  ;;
end
