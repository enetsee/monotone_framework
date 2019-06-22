open Core_kernel

module Pattern = struct
  module Basic = struct
    type ('a, 'b, 's) t =
      | Assign of string * 'a
      | Skip
      | Block of 's list
      | If of 'b * 's * 's
      | While of 'b * 's
    [@@deriving map, fold, sexp, compare]

    let trimap ~f ~g ~h x = map f g h x
    let trifold_left ~f ~g ~h ~init x = fold f g h init x
    let trifold_right = `Define_using_trifold_left
  end

  module F = struct
    include Basic
    include Trifunctor.Make (Basic)
    include Trifoldable.Make (Basic)
  end

  include F

  module Make_tritraversable (X : Monad.S) :
    Tritraversable.S with module F := F and module M := X = struct
    let tritraverse ~f ~g ~h = function
      | Assign (name, a) -> X.map ~f:(fun a -> Assign (name, a)) (f a)
      | Skip -> X.return Skip
      | Block xs -> X.(List.map ~f:h xs |> all |> map ~f:(fun xs -> Block xs))
      | If (test, ts, fs) ->
        X.(
          g test
          >>= fun test' ->
          h ts >>= fun ts' -> h fs >>= fun fs' -> return @@ If (test', ts', fs'))
      | While (test, body) ->
        X.(
          g test
          >>= fun test' ->
          h body >>= fun body' -> return @@ While (test', body'))
    ;;
  end

  module Make_tritraversable2 (X : Monad.S2) :
    Tritraversable.S2 with module F := F and module M := X = struct
    let tritraverse ~f ~g ~h = function
      | Assign (name, a) -> X.map ~f:(fun a -> Assign (name, a)) (f a)
      | Skip -> X.return Skip
      | Block xs -> X.(List.map ~f:h xs |> all |> map ~f:(fun xs -> Block xs))
      | If (test, ts, fs) ->
        X.(
          g test
          >>= fun test' ->
          h ts >>= fun ts' -> h fs >>= fun fs' -> return @@ If (test', ts', fs'))
      | While (test, body) ->
        X.(
          g test
          >>= fun test' ->
          h body >>= fun body' -> return @@ While (test', body'))
    ;;
  end

  let assign name expr = Assign (name, expr)
  let skip = Skip
  let block xs = Block xs
  let if_ test ts fs = If (test, ts, fs)
  let while_ test body = While (test, body)
end

module Fixed = Fix.Make3 (Pattern) (Arith_expr) (Bool_expr)
include Fixed
