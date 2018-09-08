
type 'a t

val run_parser : 'a t -> string -> ('a * int) option

val map : ('a -> 'b) -> 'a t -> 'b t

val pure : 'a -> 'a t

val empty : 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val (<@@>) : ('a -> 'b) -> 'a t -> 'b t

val (<**>) : 'a t -> ('a -> 'b) t -> 'b t

val (<|>) : 'a t -> 'a t -> 'a t

val (<||>) : 'a t -> (unit -> 'a t) -> 'a t

val ( *> ) : 'a t -> 'b t -> 'b t

val ( <* ) : 'a t -> 'b t -> 'a t

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
