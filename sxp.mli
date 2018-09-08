
type cons = { car: sexpr; cdr: sexpr }
and sexpr =
  | Nil
  | String of string
  | Symbol of string
  | Int of int
  | Float of float
  | Cons of cons

val cons : sexpr -> sexpr -> sexpr

val to_string : sexpr -> string

module Reader : sig
  val integer : int Parser.t
  val float : float Parser.t
  val string : string Parser.t
  val symbol : string Parser.t
  val list : sexpr Parser.t
  val expr : sexpr Parser.t
end

(* val read_string : string -> (sexpr * string) option *)
