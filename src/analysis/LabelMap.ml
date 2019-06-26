open Core_kernel
open Lang
include Map.Make_using_comparator (Stmt.Labelled.Label)
