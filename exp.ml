type 'a arguments = 'a [@@deriving show]

type rexp =
  | Var of Symbol.t
  | Lambda of Symbol.t arguments * exp
  | Let of Symbol.t * exp * exp
  | Rec of Symbol.t * exp
  | App of exp * exp arguments
  | Ascription of exp * Types.var Types.typeterm
  | Unit
  | Int of int
  | Bool of bool
  | If of exp * exp * exp
  | Nil
  | Cons of exp * exp
  | Match of exp * exp * Symbol.t * Symbol.t * exp
  | Object of (Symbol.t * exp) list
  | GetField of exp * Symbol.t

and exp = (Location.t[@opaque]) * rexp [@@deriving show { with_path = false }]
