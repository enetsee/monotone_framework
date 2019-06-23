open Core_kernel

module Fixed = struct
  module type Basic = sig
    module Pattern : Pattern_functor.S

    type 'a t =
      { pattern : 'a t Pattern.t
      ; meta : 'a
      }

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

    val hash_fold_t
      :  (Hash.state -> 'a -> Hash.state)
      -> Hash.state
      -> 'a t
      -> Hash.state

    include Functor.S with type 'a t := 'a t
    include Foldable.Basic with type 'a t := 'a t
    include Sexpable.S1 with type 'a t := 'a t
  end

  module type S = sig
    module Pattern : Pattern_functor.S

    type 'a t =
      { pattern : 'a t Pattern.t
      ; meta : 'a
      }

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

    val hash_fold_t
      :  (Hash.state -> 'a -> Hash.state)
      -> Hash.state
      -> 'a t
      -> Hash.state

    include Sexpable.S1 with type 'a t := 'a t
    include Functor.S with type 'a t := 'a t
    include Foldable.S with type 'a t := 'a t

    val pattern : 'a t -> 'a t Pattern.t
    val meta : 'a t -> 'a
    val fix : 'a -> 'a t Pattern.t -> 'a t
    val map_pattern : f:('a t Pattern.t -> 'a t Pattern.t) -> 'a t -> 'a t

    val fold_left_pattern
      :  f:('a -> 'b t Pattern.t -> 'a)
      -> init:'a
      -> 'b t
      -> 'a

    val fold_right_pattern
      :  f:('b t Pattern.t -> 'a -> 'a)
      -> init:'a
      -> 'b t
      -> 'a

    val fold_map_pattern
      :  (module Monoid.S with type t = 'a)
      -> f:('b t Pattern.t -> 'a)
      -> ?init:'a
      -> 'b t
      -> 'a

    val any_pattern
      :  pred:('a t Pattern.t -> bool)
      -> ?init:bool
      -> 'a t
      -> bool

    val all_pattern
      :  pred:('a t Pattern.t -> bool)
      -> ?init:bool
      -> 'a t
      -> bool
  end

  module type Basic2 = sig
    module Pattern : Pattern_functor.S2
    module First : S

    type ('a, 'b) t =
      { pattern : ('a First.t, ('a, 'b) t) Pattern.t
      ; meta : 'b
      }

    val compare
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('a, 'b) t
      -> ('a, 'b) t
      -> int

    val hash_fold_t
      :  (Hash.state -> 'a -> Hash.state)
      -> (Hash.state -> 'b -> Hash.state)
      -> Hash.state
      -> ('a, 'b) t
      -> Hash.state

    include Bifunctor.Basic with type ('a, 'b) t := ('a, 'b) t
    include Bifoldable.Basic with type ('a, 'b) t := ('a, 'b) t
    include Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t
  end

  module type S2 = sig
    module Pattern : Pattern_functor.S2
    module First : S

    type ('a, 'b) t =
      { pattern : ('a First.t, ('a, 'b) t) Pattern.t
      ; meta : 'b
      }

    val compare
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('a, 'b) t
      -> ('a, 'b) t
      -> int

    val hash_fold_t
      :  (Hash.state -> 'a -> Hash.state)
      -> (Hash.state -> 'b -> Hash.state)
      -> Hash.state
      -> ('a, 'b) t
      -> Hash.state

    include Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t
    include Bifunctor.S with type ('a, 'b) t := ('a, 'b) t
    include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t

    val pattern : ('a, 'b) t -> ('a First.t, ('a, 'b) t) Pattern.t
    val meta : ('a, 'b) t -> 'b
    val fix : 'b -> ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t

    val bimap_pattern
      :  f:('a First.t First.Pattern.t -> 'a First.t First.Pattern.t)
      -> g:(('a First.t, ('a, 'b) t) Pattern.t
            -> ('a First.t, ('a, 'b) t) Pattern.t)
      -> ('a, 'b) t
      -> ('a, 'b) t

    val bifold_left_pattern
      :  f:('a -> 'b First.t First.Pattern.t -> 'a)
      -> g:('a -> ('b First.t, ('b, 'c) t) Pattern.t -> 'a)
      -> init:'a
      -> ('b, 'c) t
      -> 'a

    val bifold_right_pattern
      :  f:('b First.t First.Pattern.t -> 'a -> 'a)
      -> g:(('b First.t, ('b, 'c) t) Pattern.t -> 'a -> 'a)
      -> init:'a
      -> ('b, 'c) t
      -> 'a

    val bifold_map_pattern
      :  (module Monoid.S with type t = 'a)
      -> f:('b First.t First.Pattern.t -> 'a)
      -> g:(('b First.t, ('b, 'c) t) Pattern.t -> 'a)
      -> ?init:'a
      -> ('b, 'c) t
      -> 'a

    val biany_pattern
      :  pred_first:('a First.t First.Pattern.t -> bool)
      -> pred_second:(('a First.t, ('a, 'b) t) Pattern.t -> bool)
      -> ?init:bool
      -> ('a, 'b) t
      -> bool

    val biall_pattern
      :  pred_first:('a First.t First.Pattern.t -> bool)
      -> pred_second:(('a First.t, ('a, 'b) t) Pattern.t -> bool)
      -> ?init:bool
      -> ('a, 'b) t
      -> bool
  end

  (* This _is_ ridiculous - luckily we don't need this in stanc3 *)
  module type Basic3 = sig
    module Pattern : Pattern_functor.S3
    module First : S
    module Second : S2 with module First := First

    type ('a, 'b, 'c) t =
      { pattern : ('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
      ; meta : 'c
      }

    val compare
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('c -> 'c -> int)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t
      -> int

    val hash_fold_t
      :  (Hash.state -> 'a -> Hash.state)
      -> (Hash.state -> 'b -> Hash.state)
      -> (Hash.state -> 'c -> Hash.state)
      -> Hash.state
      -> ('a, 'b, 'c) t
      -> Hash.state

    include Trifunctor.Basic with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    include Trifoldable.Basic with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    include Sexpable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  end

  module type S3 = sig
    module Pattern : Pattern_functor.S3
    module First : S
    module Second : S2 with module First := First

    type ('a, 'b, 'c) t =
      { pattern : ('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
      ; meta : 'c
      }

    val compare
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('c -> 'c -> int)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t
      -> int

    val hash_fold_t
      :  (Hash.state -> 'a -> Hash.state)
      -> (Hash.state -> 'b -> Hash.state)
      -> (Hash.state -> 'c -> Hash.state)
      -> Hash.state
      -> ('a, 'b, 'c) t
      -> Hash.state

    include Sexpable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    include Trifunctor.S with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    include Trifoldable.S with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

    val pattern
      :  ('a, 'b, 'c) t
      -> ('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t

    val meta : ('a, 'b, 'c) t -> 'c

    val fix
      :  'c
      -> ('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
      -> ('a, 'b, 'c) t

    (* module Make_tritraversable(X: Monad.S) : Tritraversable.S with module M
       := X and module F := F module Make_tritraversable2(X: Monad.S2) :
       Tritraversable.S2 with module M := X and module F := F *)

    val trimap_pattern
      :  f:('a First.t First.Pattern.t -> 'a First.t First.Pattern.t)
      -> g:(('a First.t, ('a, 'b) Second.t) Second.Pattern.t
            -> ('a First.t, ('a, 'b) Second.t) Second.Pattern.t)
      -> h:(('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
            -> ('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t

    val trifold_left_pattern
      :  f:('a -> 'b First.t First.Pattern.t -> 'a)
      -> g:('a -> ('b First.t, ('b, 'c) Second.t) Second.Pattern.t -> 'a)
      -> h:('a
            -> ('b First.t, ('b, 'c) Second.t, ('b, 'c, 'd) t) Pattern.t
            -> 'a)
      -> init:'a
      -> ('b, 'c, 'd) t
      -> 'a

    val trifold_right_pattern
      :  f:('b First.t First.Pattern.t -> 'a -> 'a)
      -> g:(('b First.t, ('b, 'c) Second.t) Second.Pattern.t -> 'a -> 'a)
      -> h:(('b First.t, ('b, 'c) Second.t, ('b, 'c, 'd) t) Pattern.t
            -> 'a
            -> 'a)
      -> init:'a
      -> ('b, 'c, 'd) t
      -> 'a

    val trifold_map_pattern
      :  (module Monoid.S with type t = 'a)
      -> f:('b First.t First.Pattern.t -> 'a)
      -> g:(('b First.t, ('b, 'c) Second.t) Second.Pattern.t -> 'a)
      -> h:(('b First.t, ('b, 'c) Second.t, ('b, 'c, 'd) t) Pattern.t -> 'a)
      -> ?init:'a
      -> ('b, 'c, 'd) t
      -> 'a

    val biany_pattern
      :  pred_first:('a First.t First.Pattern.t -> bool)
      -> pred_second:(('a First.t, ('a, 'b) Second.t) Second.Pattern.t -> bool)
      -> pred_third:(('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
                     -> bool)
      -> ?init:bool
      -> ('a, 'b, 'c) t
      -> bool

    val biall_pattern
      :  pred_first:('a First.t First.Pattern.t -> bool)
      -> pred_second:(('a First.t, ('a, 'b) Second.t) Second.Pattern.t -> bool)
      -> pred_third:(('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
                     -> bool)
      -> ?init:bool
      -> ('a, 'b, 'c) t
      -> bool
  end

  module Make (X : Basic) :
    S with module Pattern := X.Pattern and type 'a t := 'a X.t = struct
    let map = X.map

    include Foldable.Make (X)

    let sexp_of_t = X.sexp_of_t
    let t_of_sexp = X.t_of_sexp
    let compare = X.compare
    let hash_fold_t = X.hash_fold_t
    let pattern { X.pattern; _ } = pattern
    let meta { X.meta; _ } = meta
    let fix meta pattern = { X.pattern; meta }

    let rec map_pattern ~f { X.pattern; meta } =
      { X.pattern = f @@ X.Pattern.map ~f:(map_pattern ~f) pattern; meta }
    ;;

    let rec fold_left_pattern ~f ~init { X.pattern; _ } =
      X.Pattern.fold_left
        ~f:(fun accu x -> fold_left_pattern ~f ~init:accu x)
        ~init:(f init pattern)
        pattern
    ;;

    let rec fold_right_pattern ~f ~init { X.pattern; _ } =
      f pattern
      @@ X.Pattern.fold_right
           ~f:(fun x accu -> fold_right_pattern ~f ~init:accu x)
           ~init
           pattern
    ;;

    let fold_map_pattern
        (type a)
        (module M : Monoid.S with type t = a)
        ~f
        ?init:(empty = M.empty)
        x
      =
      fold_right_pattern ~f:(fun x accu -> M.combine accu @@ f x) ~init:empty x
    ;;

    let any_pattern ~pred ?init x =
      fold_map_pattern (module Monoid.Bool_or) ~f:pred ?init x
    ;;

    let all_pattern ~pred ?init x =
      fold_map_pattern (module Monoid.Bool_and) ~f:pred ?init x
    ;;
  end

  module Make2 (X : Basic2) :
    S2
    with module Pattern := X.Pattern
     and module First := X.First
     and type ('a, 'b) t := ('a, 'b) X.t = struct
    include Bifunctor.Make (X)
    include Bifoldable.Make (X)

    let pattern { X.pattern; _ } = pattern
    let meta { X.meta; _ } = meta
    let fix meta pattern = { X.pattern; meta }
    let sexp_of_t = X.sexp_of_t
    let t_of_sexp = X.t_of_sexp
    let compare = X.compare
    let hash_fold_t = X.hash_fold_t

    let rec bimap_pattern ~f ~g { X.pattern; meta } =
      { X.pattern =
          g
          @@ X.Pattern.bimap
               ~f:(X.First.map_pattern ~f)
               ~g:(bimap_pattern ~f ~g)
               pattern
      ; meta
      }
    ;;

    let rec bifold_left_pattern ~f ~g ~init { X.pattern; _ } =
      X.Pattern.bifold_left
        ~f:(fun accu x -> X.First.fold_left_pattern ~f ~init:accu x)
        ~g:(fun accu x -> bifold_left_pattern ~f ~g ~init:accu x)
        ~init:(g init pattern)
        pattern
    ;;

    let rec bifold_right_pattern ~f ~g ~init { X.pattern; _ } =
      X.Pattern.bifold_right
        ~f:(fun x accu -> X.First.fold_right_pattern ~f ~init:accu x)
        ~g:(fun x accu -> bifold_right_pattern ~f ~g ~init:accu x)
        ~init
        pattern
      |> g pattern
    ;;

    let bifold_map_pattern
        (type a)
        (module M : Monoid.S with type t = a)
        ~f
        ~g
        ?init:(empty = M.empty)
        x
      =
      bifold_right_pattern
        ~f:(fun x accu -> M.combine accu @@ f x)
        ~g:(fun x accu -> M.combine accu @@ g x)
        ~init:empty
        x
    ;;

    let biany_pattern ~pred_first ~pred_second ?init x =
      bifold_map_pattern
        (module Monoid.Bool_or)
        ~f:pred_first
        ~g:pred_second
        ?init
        x
    ;;

    let biall_pattern ~pred_first ~pred_second ?init x =
      bifold_map_pattern
        (module Monoid.Bool_and)
        ~f:pred_first
        ~g:pred_second
        ?init
        x
    ;;
  end

  module Make3 (X : Basic3) :
    S3
    with module First := X.First
     and module Second := X.Second
     and module Pattern := X.Pattern
     and type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t = struct
    include Trifunctor.Make (X)
    include Trifoldable.Make (X)

    let pattern { X.pattern; _ } = pattern
    let meta { X.meta; _ } = meta
    let fix meta pattern = { X.pattern; meta }
    let sexp_of_t = X.sexp_of_t
    let t_of_sexp = X.t_of_sexp
    let compare = X.compare
    let hash_fold_t = X.hash_fold_t

    let rec trimap_pattern ~f ~g ~h { X.pattern; meta } =
      { X.pattern =
          h
          @@ X.Pattern.trimap
               ~f:(X.First.map_pattern ~f)
               ~g:(X.Second.bimap_pattern ~f ~g)
               ~h:(trimap_pattern ~f ~g ~h)
               pattern
      ; meta
      }
    ;;

    let rec trifold_left_pattern ~f ~g ~h ~init { X.pattern; _ } =
      X.Pattern.trifold_left
        ~f:(fun accu x -> X.First.fold_left_pattern ~f ~init:accu x)
        ~g:(fun accu x -> X.Second.bifold_left_pattern ~f ~g ~init:accu x)
        ~h:(fun accu x -> trifold_left_pattern ~f ~g ~h ~init:accu x)
        ~init:(h init pattern)
        pattern
    ;;

    let rec trifold_right_pattern ~f ~g ~h ~init { X.pattern; _ } =
      X.Pattern.trifold_right
        ~f:(fun x accu -> X.First.fold_right_pattern ~f ~init:accu x)
        ~g:(fun x accu -> X.Second.bifold_right_pattern ~f ~g ~init:accu x)
        ~h:(fun x accu -> trifold_right_pattern ~f ~g ~h ~init:accu x)
        ~init
        pattern
      |> h pattern
    ;;

    let trifold_map_pattern
        (type a)
        (module M : Monoid.S with type t = a)
        ~f
        ~g
        ~h
        ?init:(empty = M.empty)
        x
      =
      trifold_right_pattern
        ~f:(fun x accu -> M.combine accu @@ f x)
        ~g:(fun x accu -> M.combine accu @@ g x)
        ~h:(fun x accu -> M.combine accu @@ h x)
        ~init:empty
        x
    ;;

    let biany_pattern ~pred_first ~pred_second ~pred_third ?init x =
      trifold_map_pattern
        (module Monoid.Bool_or)
        ~f:pred_first
        ~g:pred_second
        ~h:pred_third
        ?init
        x
    ;;

    let biall_pattern ~pred_first ~pred_second ~pred_third ?init x =
      trifold_map_pattern
        (module Monoid.Bool_and)
        ~f:pred_first
        ~g:pred_second
        ~h:pred_third
        ?init
        x
    ;;
  end
end

module type S = sig
  module Pattern : Pattern_functor.S
  include Fixed.S with module Pattern := Pattern
end

module type S2 = sig
  module Pattern : Pattern_functor.S2
  module First : S
  include Fixed.S2 with module Pattern := Pattern and module First := First
end

module type S3 = sig
  module Pattern : Pattern_functor.S3
  module First : S
  module Second : S2 with module First := First

  include
    Fixed.S3
    with module Pattern := Pattern
     and module First := First
     and module Second := Second
end

module Make (Pattern : Pattern_functor.S) : S with module Pattern := Pattern =
struct
  module Basic = struct
    module Pattern = Pattern

    type 'a t =
      { pattern : 'a t Pattern.t
      ; meta : 'a [@compare_sexp_opaque] [@hash_fold_sexp_opaque]
      }
    [@@deriving sexp, compare, hash]

    let rec map ~f { pattern; meta } =
      { pattern = Pattern.map ~f:(map ~f) pattern; meta = f meta }
    ;;

    let rec fold_left ~f ~init { pattern; meta } =
      Pattern.fold_left
        ~f:(fun accu x -> fold_left ~f ~init:accu x)
        ~init:(f init meta)
        pattern
    ;;

    let fold_right = `Define_using_fold_left
  end

  include Basic
  include Fixed.Make (Basic)
end

module Make2 (Pattern : Pattern_functor.S2) (First : S) :
  S2 with module Pattern := Pattern and module First := First = struct
  module Basic = struct
    module Pattern = Pattern
    module First = First

    type ('a, 'b) t =
      { pattern : ('a First.t, ('a, 'b) t) Pattern.t
      ; meta : 'b [@compare_sexp_opaque] [@hash_fold_sexp_opaque]
      }
    [@@deriving sexp, compare, hash]

    let rec bimap ~f ~g { pattern; meta } =
      { pattern = Pattern.bimap ~f:(First.map ~f) ~g:(bimap ~f ~g) pattern
      ; meta = g meta
      }
    ;;

    let rec bifold_left ~f ~g ~init { pattern; meta } =
      Pattern.bifold_left
        ~f:(fun accu x -> First.fold_left ~f ~init:accu x)
        ~g:(fun accu x -> bifold_left ~f ~g ~init:accu x)
        ~init:(g init meta)
        pattern
    ;;

    let bifold_right = `Define_using_bifold_left
  end

  include Basic
  include Fixed.Make2 (Basic)
end

module Make3
    (Pattern : Pattern_functor.S3)
    (First : S)
    (Second : S2 with module First := First) :
  S3
  with module Pattern := Pattern
   and module First := First
   and module Second := Second = struct
  module Basic = struct
    module Pattern = Pattern
    module First = First
    module Second = Second

    type ('a, 'b, 'c) t =
      { pattern : ('a First.t, ('a, 'b) Second.t, ('a, 'b, 'c) t) Pattern.t
      ; meta : 'c [@compare_sexp_opaque] [@hash_fold_sexp_opaque]
      }
    [@@deriving sexp, compare, hash]

    let rec trimap ~f ~g ~h { pattern; meta } =
      { pattern =
          Pattern.trimap
            ~f:(First.map ~f)
            ~g:(Second.bimap ~f ~g)
            ~h:(trimap ~f ~g ~h)
            pattern
      ; meta = h meta
      }
    ;;

    let rec trifold_left ~f ~g ~h ~init { pattern; meta } =
      Pattern.trifold_left
        ~f:(fun accu x -> First.fold_left ~f ~init:accu x)
        ~g:(fun accu x -> Second.bifold_left ~f ~g ~init:accu x)
        ~h:(fun accu x -> trifold_left ~f ~g ~h ~init:accu x)
        ~init:(h init meta)
        pattern
    ;;

    let trifold_right = `Define_using_trifold_left
  end

  include Basic
  include Fixed.Make3 (Basic)
end
