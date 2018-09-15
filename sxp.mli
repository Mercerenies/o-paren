
type cons = { car: sexpr; cdr: sexpr }
and fun_type = { parms: string list; body: sexpr list }
and sexpr =
  | Nil
  | String of string
  | Symbol of string
  | Int of int
  | Float of float
  | Cons of cons
  | Function of fun_type

type t = sexpr

val cons : sexpr -> sexpr -> sexpr

val make_function : string list -> sexpr list -> sexpr

val to_string : sexpr -> string

module Reader : sig
  val integer : int Parser.t
  val float : float Parser.t
  val string : string Parser.t
  val symbol : string Parser.t
  val list : sexpr Parser.t
  val expr : sexpr Parser.t
  val quoted : sexpr Parser.t
end

val read_expr : string -> sexpr option
val read_exprs : string -> sexpr list

module SList : sig

  type 'a expr_list = 'a list * 'a

  val from_list_generic : ('a -> 'a -> 'a) -> 'a expr_list -> 'a

  val from_list : sexpr expr_list -> sexpr

  val to_list : sexpr -> sexpr expr_list

  val nil_term : sexpr list -> sexpr expr_list

end
