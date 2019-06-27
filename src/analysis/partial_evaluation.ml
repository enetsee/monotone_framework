open Core_kernel
open Lang
open Arith_expr

let eval_op = function
  | Arith_expr.Plus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )
  | Div -> ( / )
;;

let apply_op op e1 e2 =
  match Fixed.pattern e1, Fixed.pattern e2 with
  | Lit l1, Lit l2 -> eval_op op l1 l2 |> Arith_expr.Pattern.lit |> Some
  | _ -> None
;;

let rec eval_arith_expr ~env ?cont:(k = fun x -> x) { Fixed.pattern; meta } =
  match pattern with
  | Var var ->
    StringMap.find env var
    |> Option.value_map ~default:(Arith_expr.Pattern.Var var) ~f:Fixed.pattern
    |> Fixed.fix meta
    |> k
  | Lit n -> Arith_expr.Pattern.Lit n |> Fixed.fix meta |> k
  | Binop (e1, op, e2) ->
    eval_arith_expr ~env e1 ~cont:(fun e1' ->
        eval_arith_expr ~env e2 ~cont:(fun e2' ->
            apply_op op e1' e2'
            |> Option.value ~default:(Binop (e1', op, e2'))
            |> Fixed.fix meta
            |> k))
;;
