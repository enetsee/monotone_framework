open Core_kernel
open Lang
open Common



type t = Stmt.Labelled.t

module Label = Stmt.Labelled.Label
module LabelSet = Set.Make_using_comparator(Label)

module Property = struct
    module Basic = struct
      type t = string * int option [@@deriving compare, hash, sexp, sexp_of]
    end
    include Basic
    include Comparator.Make (Basic)
end

module PropertySet = Set.Make_using_comparator(Property)

module LabelMap = Map.Make_using_comparator(Label)

type entry_exit_properties = 
  { entry : PropertySet.t 
  ; exit : PropertySet.t 
  }

type result = entry_exit_properties LabelMap.t


let label_of (x : t) : Label.t = Stmt.Labelled.label_of x



let is_assignment_to var ({ Stmt.pattern; _ } : t)=
  match pattern with
  | Stmt.Pattern.Assign (var', _) -> var' = var
  | _ -> false
;;

let kill (blocks : t list) ({ Stmt.pattern; _ }:t) =
  match pattern with
  | Stmt.Pattern.Assign (x, _) ->
    List.fold_left
      blocks
      ~init:(PropertySet.singleton (x, None))
      ~f:(fun accu stmt ->
        if is_assignment_to x stmt
        then PropertySet.add accu (x, Some (label_of stmt))
        else accu)
  | _ -> PropertySet.empty
;;

let gen ({ Stmt.pattern; _ } as x : t) =
  match pattern with
  | Stmt.Pattern.Assign (var, _) -> 
      PropertySet.singleton (var, Some (label_of x))

  | _ -> 
    PropertySet.empty
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
let labels_2_5 : LabelSet.t= labels example_2_5
let blocks_2_5 = blocks assocs_2_5 labels_2_5
let initial_2_5 : Label.t = initial example_2_5
let flow_2_5 = flow example_2_5



(* let kills = labels_2_5 |> IntSet.to_list |> List.filter_map ~f:(fun label ->
   Associations.find_stmt assocs_2_5 label |> Option.map ~f:(fun s -> label,
   kill labelled_blocks_2_5 s |> AExpSet.to_list) ) ;;

   let gens = labels_2_5 |> IntSet.to_list |> List.filter_map ~f:(fun label ->
   Associations.find_stmt assocs_2_5 label |> Option.map ~f:(fun s -> label,
   gen s |> AExpSet.to_list)) ;; *)

(* == Solve ================================================================= *)
