
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
  val read_integer : string -> (int * string) option
  val read_float : string -> (float * string) option
  val read_string : string -> (string * string) option
  val read_symbol : string -> (string * string) option
  val read_list : string -> (sexpr * string) option
  val read_expr : string -> (sexpr * string) option
end

(* val read_string : string -> (sexpr * string) option *)
