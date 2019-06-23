open Lang

let test_expr = Arith_expr.(
    plus_ (var_ "x") (lit_ 1)
)
let main () = ()
let () = main ()
