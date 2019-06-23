open Lang

type t =
  { arith_exprs : Arith_expr.Unlabelled.t IntMap.t
  ; bool_exprs : Bool_expr.Unlabelled.t IntMap.t
  ; stmts : Stmt.Unlabelled.t IntMap.t
  }

let empty =
  { arith_exprs = IntMap.empty
  ; bool_exprs = IntMap.empty
  ; stmts = IntMap.empty
  }
;;

let add_arith_expr { arith_exprs; bool_exprs; stmts } arith_expr =
  let key = Arith_expr.Labelled.label_of arith_expr
  and data = Arith_expr.Unlabelled.unlabel arith_expr in
  { arith_exprs = IntMap.add_exn ~key ~data arith_exprs; bool_exprs; stmts }
;;

let add_bool_expr { arith_exprs; bool_exprs; stmts } bool_expr =
  let key = Bool_expr.Labelled.label_of bool_expr
  and data = Bool_expr.Unlabelled.unlabel bool_expr in
  { arith_exprs; bool_exprs = IntMap.add_exn ~key ~data bool_exprs; stmts }
;;

let add_stmt { arith_exprs; bool_exprs; stmts } stmt =
  let key = Stmt.Labelled.label_of stmt
  and data = Stmt.Unlabelled.unlabel stmt in
  { arith_exprs; bool_exprs; stmts = IntMap.add_exn ~key ~data stmts }
;;

let find_arith_expr { arith_exprs; _ } lbl = IntMap.find arith_exprs lbl
let find_bool_expr { bool_exprs; _ } lbl = IntMap.find bool_exprs lbl
let find_stmt { stmts; _ } lbl = IntMap.find stmts lbl
let stmts { stmts; _ } = IntMap.to_alist stmts
let arith_exprs { arith_exprs; _ } = IntMap.to_alist arith_exprs
let bool_exprs { bool_exprs; _ } = IntMap.to_alist bool_exprs
