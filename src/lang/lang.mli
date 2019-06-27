module Arith_expr : sig
  include module type of Arith_expr
end

module Bool_expr : sig
  include module type of Bool_expr
end

module Stmt : sig
  include module type of Stmt
end
