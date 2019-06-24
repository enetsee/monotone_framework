open Core_kernel
open Lang
module AExpMap = Map.Make_using_comparator (Arith_expr.Labelled.Label)
module BExpMap = Map.Make_using_comparator (Bool_expr.Labelled.Label)
module StmtMap = Map.Make_using_comparator (Stmt.Labelled.Label)

type t =
  { arith_exprs : Arith_expr.Labelled.t AExpMap.t
  ; bool_exprs : Bool_expr.Labelled.t BExpMap.t
  ; stmts : Stmt.Labelled.t StmtMap.t
  }

let empty =
  { arith_exprs = AExpMap.empty
  ; bool_exprs = BExpMap.empty
  ; stmts = StmtMap.empty
  }
;;

let add_arith_expr { arith_exprs; bool_exprs; stmts } arith_expr =
  let key = Arith_expr.Labelled.label_of arith_expr
  and data = arith_expr in
  { arith_exprs = AExpMap.add_exn ~key ~data arith_exprs; bool_exprs; stmts }
;;

let add_bool_expr { arith_exprs; bool_exprs; stmts } bool_expr =
  let key = Bool_expr.Labelled.label_of bool_expr
  and data = bool_expr in
  { arith_exprs; bool_exprs = BExpMap.add_exn ~key ~data bool_exprs; stmts }
;;

let add_stmt { arith_exprs; bool_exprs; stmts } stmt =
  let key = Stmt.Labelled.label_of stmt
  and data = stmt in
  { arith_exprs; bool_exprs; stmts = StmtMap.add_exn ~key ~data stmts }
;;

let find_arith_expr { arith_exprs; _ } lbl = AExpMap.find arith_exprs lbl
let find_bool_expr { bool_exprs; _ } lbl = BExpMap.find bool_exprs lbl
let find_stmt { stmts; _ } lbl = StmtMap.find stmts lbl
let stmts { stmts; _ } = StmtMap.to_alist stmts
let arith_exprs { arith_exprs; _ } = AExpMap.to_alist arith_exprs
let bool_exprs { bool_exprs; _ } = BExpMap.to_alist bool_exprs
let stmt_map { stmts; _ } = stmts
let arith_expr_map { arith_exprs; _ } = arith_exprs
let bool_expr_map { bool_exprs; _ } = bool_exprs
