open Core_kernel

include Label.Make (struct
  type t = int [@@deriving hash, sexp_of, of_sexp, compare]
end)
