open Core_kernel
open Lang
open Common

let kill aexp_star = function
  | Stmt.Pattern.Assign (x, _) ->
    aexp_star
    |> List.filter ~f:(fun (_, expr) ->
           List.exists ~f:(fun y -> x = y) @@ vars_pattern expr)
  | _ -> []
;;

let gen = function
  | Stmt.Pattern.If (test, _, _) | Stmt.Pattern.While (test, _) ->
    associate_bool_expr test |> non_trivial_arith_expr
  | Assign (x, a) ->
    associate_arith_expr a
    |> non_trivial_arith_expr
    |> List.filter ~f:(fun (_, expr) ->
           List.for_all ~f:(fun y -> x <> y) @@ vars_pattern expr)
  | _ -> []
;;
