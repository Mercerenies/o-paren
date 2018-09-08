
type 'a parser

val run_parser : 'a parser -> string -> ('a * int) option

val map : ('a -> 'b) -> 'a parser -> 'b parser

val pure : 'a -> 'a parser

val empty : 'a parser

val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser

val (<*>) : ('a -> 'b) parser -> 'a parser -> 'b parser

val (<@@>) : ('a -> 'b) -> 'a parser -> 'b parser

val (<|>) : 'a parser -> 'a parser -> 'a parser

val sequence : 'a parser list -> 'a list parser

val parse_regexp : Pcre.regexp -> Pcre.substrings parser

val skip_whitespace : unit parser

val satisfy : (char -> bool) -> char parser

val char : char -> char parser

val any_char : char parser

val string : string -> string parser

val one_of : 'a parser list -> 'a parser

val some : 'a parser -> 'a list parser

val many : 'a parser -> 'a list parser
