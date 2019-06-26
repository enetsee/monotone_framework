open Core_kernel
open Lang

let eval_op = function
  | Arith_expr.Plus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )
  | Div -> ( / )
;;

let apply_op op e1 e2 =
  match Arith_expr.pattern e1, Arith_expr.pattern e2 with
  | Lit l1, Lit l2 -> eval_op op l1 l2 |> Arith_expr.Pattern.lit |> Some
  | _ -> None
;;

let rec eval_arith_expr
    ~env ?cont:(k = fun x -> x) { Arith_expr.pattern; meta }
  =
  match pattern with
  | Var var ->
    StringMap.find env var
    |> Option.value_map
         ~default:(Arith_expr.Pattern.Var var)
         ~f:Arith_expr.pattern
    |> Arith_expr.fix meta
    |> k
  | Lit n -> Arith_expr.Pattern.Lit n |> Arith_expr.fix meta |> k
  | Binop (e1, op, e2) ->
    eval_arith_expr ~env e1 ~cont:(fun e1' ->
        eval_arith_expr ~env e2 ~cont:(fun e2' ->
            apply_op op e1' e2'
            |> Option.value ~default:(Binop (e1', op, e2'))
            |> Arith_expr.fix meta
            |> k))
;;
