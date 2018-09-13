
open Cat

type 'a t

val run_parser : 'a t -> string -> ('a * int) option

module ParserFunctor : FUNCTOR with type 'a f = 'a t
module ParserApplicative : APPLICATIVE with type 'a f = 'a t
module ParserMonad : MONAD with type 'a f = 'a t

val empty : 'a t

val (<**>) : 'a t -> ('a -> 'b) t -> 'b t

val (<|>) : 'a t -> 'a t -> 'a t

val (<||>) : 'a t -> (unit -> 'a t) -> 'a t

val sequence : 'a t list -> 'a list t

val regexp : Pcre.regexp -> Pcre.substrings t

val skip_whitespace : unit t

val satisfy : (char -> bool) -> char t

val char : char -> char t

val not_char : char -> char t

val any_char : char t

val string : string -> string t

val one_of : 'a t list -> 'a t

val some : 'a t -> 'a list t

val many : 'a t -> 'a list t

val optional : 'a t -> 'a option t
