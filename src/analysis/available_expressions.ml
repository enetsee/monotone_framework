open Core_kernel
open Lang
open Common

let kill aexp_star { Stmt.pattern; _ } =
  match pattern with
  | Stmt.Pattern.Assign (x, _) ->
    aexp_star
    |> AExpSet.filter ~f:(fun expr ->
           List.exists ~f:(fun y -> x = y) @@ vars expr)
  | _ -> AExpSet.empty
;;

let gen ({ Stmt.pattern; _ } : Stmt.Unlabelled.t) =
  match pattern with
  | Stmt.Pattern.If (test, _, _) | Stmt.Pattern.While (test, _) ->
    Bool_expr.bifold_right_pattern
      ~g:(fun _ accu -> accu)
      ~init:[]
      test
      ~f:(fun pattern accu ->
        let expr = Arith_expr.fix () pattern in
        if trivial expr then accu else expr :: accu)
    |> AExpSet.of_list
  | Assign (x, a) ->
    Arith_expr.fold_right_pattern ~init:[] a ~f:(fun pattern accu ->
        let expr = Arith_expr.fix () pattern in
        if trivial expr
        then accu
        else if List.exists ~f:(fun y -> x = y) @@ vars expr
        then accu
        else expr :: accu)
    |> AExpSet.of_list
  | _ -> AExpSet.empty
;;

(* == Examples ============================================================== *)
let example_2_5 =
  Stmt.(
    block_
      [ assign_ "x" Arith_expr.(plus_ (var_ "a") (var_ "b"))
      ; assign_ "y" Arith_expr.(mult_ (var_ "a") (var_ "b"))
      ; while__
          Bool_expr.(
            gt_ (Arith_expr.var_ "y") Arith_expr.(plus_ (var_ "a") (var_ "b")))
          (block_
             [ assign_ "a" Arith_expr.(plus_ (var_ "a") (lit_ 1))
             ; assign_ "x" Arith_expr.(plus_ (var_ "a") (var_ "b"))
             ])
      ]
    |> Labelled.label)
;;

let assocs_2_5 = associate example_2_5
let labels_2_5 = labels example_2_5
let blocks_2_5 = blocks assocs_2_5 labels_2_5
let aexp_2_5 = non_trivial_arith_expr assocs_2_5
let initial_2_5 = initial example_2_5
let flow_2_5 = flow example_2_5

let kills =
  labels_2_5
  |> IntSet.to_list
  |> List.filter_map ~f:(fun label ->
         Associations.find_stmt assocs_2_5 label
         |> Option.map ~f:(fun s -> label, kill aexp_2_5 s |> AExpSet.to_list)
     )
;;

let gens =
  labels_2_5
  |> IntSet.to_list
  |> List.filter_map ~f:(fun label ->
         Associations.find_stmt assocs_2_5 label
         |> Option.map ~f:(fun s -> label, gen s |> AExpSet.to_list))
;;

(* == Solve ================================================================= *)

let bottom = aexp_2_5
let i = AExpSet.empty
let e = IntSet.singleton initial_2_5
let f = flow_2_5
let leq a b = AExpSet.is_subset b ~of_:a

let lub a b = AExpSet.inter a b
let w = f

type props = 
  { flow : (int * int ) list 
  ; i :  AExpSet.t 
  ; bottom : AExpSet.t 
  ; initial_label : int 
  ; associations : int -> Stmt.Unlabelled.t option
  }

let analysis =
  IntMap.of_alist_exn
    (List.map ~f:(fun (l, _) -> l, if l = initial_2_5 then i else bottom) f)
;;

let tf bottom assocs analysis label =
  match Associations.find_stmt assocs label , IntMap.find analysis label with
  | Some stmt , Some aexps ->
    let kills = kill bottom stmt
    and gens = gen stmt in
    AExpSet.diff aexps kills 
    |> AExpSet.union gens
  | _ -> failwith "can't happen"
;;

let rec enqueue label accu = function
  | [] -> accu
  | (l, l') :: rest when l = label -> enqueue label ((l, l') :: accu) rest
  | _ :: rest -> enqueue label accu rest
;;

let update_analysis analysis label xs = 
  IntMap.update analysis label 
    ~f:(fun aexp_opt ->
        aexp_opt
        |> Option.value ~default:AExpSet.empty
        |> lub xs
    )
            
let rec aux flow analysis = function
  | [] -> analysis
  | (l, l') :: rest ->
    let tf_l = tf aexp_2_5 assocs_2_5 analysis l 
    and aexp_l' = IntMap.find_exn analysis l' in
    if not (leq tf_l aexp_l')
    then (
      let analysis' = update_analysis analysis l' tf_l 
      and w = enqueue l' rest flow in
      aux flow analysis' w)
    else aux flow analysis rest
;;

let analysis' = aux f analysis w

let entry =
  List.map ~f:fst f
  |> List.sort ~compare:(fun x y -> x - y)
  |> List.map ~f:(fun lbl ->
         lbl, IntMap.find_exn analysis' lbl |> AExpSet.to_list)
;;

let exit =
  List.map ~f:fst f
  |> List.sort ~compare:(fun x y -> x - y)
  |> List.map ~f:(fun l ->
         let tf_l = tf aexp_2_5 assocs_2_5 analysis' l in
         l, AExpSet.to_list tf_l)
;;
