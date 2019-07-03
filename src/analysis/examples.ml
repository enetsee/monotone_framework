open Lang

let example_live_variables =
  Stmt.(
    Fixed.(
      block_
        [ assign_ "x" Arith_expr.Fixed.(lit_ 2)
        ; assign_ "y" Arith_expr.Fixed.(lit_ 4)
        ; assign_ "x" Arith_expr.Fixed.(lit_ 1)
        ; if__
            Bool_expr.Fixed.(
              gt_ Arith_expr.Fixed.(var_ "y") Arith_expr.Fixed.(var_ "x"))
            (assign_ "z" Arith_expr.Fixed.(var_ "y"))
            (assign_ "z" Arith_expr.Fixed.(mult_ (var_ "y") (var_ "y")))
        ; assign_ "x" Arith_expr.Fixed.(var_ "z")
        ])
    |> Labelled.label)
;;

let example_reaching_definitions =
  Stmt.(
    Fixed.(
      block_
        [ assign_ "x" Arith_expr.Fixed.(lit_ 5)
        ; assign_ "y" Arith_expr.Fixed.(lit_ 1)
        ; while__
            Bool_expr.Fixed.(
              gt_ (Arith_expr.Fixed.var_ "x") Arith_expr.Fixed.(lit_ 1))
            (block_
               [ assign_ "y" Arith_expr.Fixed.(mult_ (var_ "x") (var_ "y"))
               ; assign_ "x" Arith_expr.Fixed.(minus_ (var_ "x") (lit_ 1))
               ])
        ])
    |> Labelled.label)
;;

let example_available_expressions =
  Stmt.(
    Fixed.(
      block_
        [ assign_ "x" Arith_expr.Fixed.(plus_ (var_ "a") (var_ "b"))
        ; assign_ "y" Arith_expr.Fixed.(mult_ (var_ "a") (var_ "b"))
        ; while__
            Bool_expr.Fixed.(
              gt_
                (Arith_expr.Fixed.var_ "y")
                Arith_expr.Fixed.(plus_ (var_ "a") (var_ "b")))
            (block_
               [ assign_ "a" Arith_expr.Fixed.(plus_ (var_ "a") (lit_ 1))
               ; assign_ "x" Arith_expr.Fixed.(plus_ (var_ "a") (var_ "b"))
               ])
        ])
    |> Labelled.label)
;;

let example_constant_propagation =
  Stmt.Fixed.(
    block_
      [ assign_ "x" Arith_expr.Fixed.(plus_ (lit_ 2) (lit_ 2))
      ; assign_ "y" Arith_expr.Fixed.(mult_ (lit_ 1) (var_ "x"))
      ; assign_ "z" Arith_expr.Fixed.(lit_ 4)
      ; while__
          Bool_expr.Fixed.(
            gt_ Arith_expr.Fixed.(var_ "z") Arith_expr.Fixed.(var_ "y"))
          (block_
             [ assign_ "z" Arith_expr.Fixed.(minus_ (var_ "z") (lit_ 1))
             ; skip_
             ])
      ]
    |> Stmt.Labelled.label)
;;
