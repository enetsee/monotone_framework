open Lang

type t =
  { arith_exprs : Arith_expr.Labelled.t Arith_expr.Pattern.t IntMap.t
  ; bool_exprs :
      (Arith_expr.Labelled.t, Bool_expr.Labelled.t) Bool_expr.Pattern.t
      IntMap.t
  ; stmts :
      ( Arith_expr.Labelled.t
      , Bool_expr.Labelled.t
      , Stmt.Labelled.t )
      Stmt.Pattern.t
      IntMap.t
  }

let empty =
  { arith_exprs = IntMap.empty
  ; bool_exprs = IntMap.empty
  ; stmts = IntMap.empty
  }
;;

let add_arith_expr arith_expr { arith_exprs; bool_exprs; stmts } =
  let label = Arith_expr.Labelled.label_of arith_expr
  and pattern = Arith_expr.pattern arith_expr in
  { arith_exprs = IntMap.add_exn ~key:label ~data:pattern arith_exprs
  ; bool_exprs
  ; stmts
  }
;;

let add_bool_expr bool_expr { arith_exprs; bool_exprs; stmts } =
  let label = Bool_expr.Labelled.label_of bool_expr
  and pattern = Bool_expr.pattern bool_expr in
  { arith_exprs
  ; bool_exprs = IntMap.add_exn ~key:label ~data:pattern bool_exprs
  ; stmts
  }
;;

let add_stmt stmt { arith_exprs; bool_exprs; stmts } =
  let label = Stmt.Labelled.label_of stmt
  and pattern = Stmt.pattern stmt in
  { arith_exprs
  ; bool_exprs
  ; stmts = IntMap.add_exn ~key:label ~data:pattern stmts
  }
;;

let find_arith_expr { arith_exprs; _ } lbl = IntMap.find arith_exprs lbl
let find_bool_expr { bool_exprs; _ } lbl = IntMap.find bool_exprs lbl
let find_stmt { stmts; _ } lbl = IntMap.find stmts lbl
let stmts { stmts; _ } = IntMap.to_alist stmts
let arith_exprs { arith_exprs; _ } = IntMap.to_alist arith_exprs
let bool_exprs { bool_exprs; _ } = IntMap.to_alist bool_exprs
