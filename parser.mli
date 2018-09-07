
type 'a parser

val run_parser : 'a parser -> string -> ('a * int) option

val map : ('a -> 'b) -> 'a parser -> 'b parser

val pure : 'a -> 'a parser

val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser
