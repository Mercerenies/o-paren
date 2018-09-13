
val drop_string : int -> string -> string

val first : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)

val second : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)

val safe_get : string -> int -> char option

val cons : 'a -> 'a list -> 'a list
