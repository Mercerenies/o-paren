
open Extlib
open ExtString
open BatPervasives
open Pcre
open Parser

type cons = { car: sexpr; cdr: sexpr }
and sexpr =
  | Nil
  | String of string
  | Symbol of string
  | Int of int
  | Float of float
  | Cons of cons

let cons x y = Cons { car = x; cdr = y }

let cons_append xs y = List.fold_right cons xs y

let rec to_string x = match x with
  | Nil -> "()"
  | String str -> "\"" ^ str ^ "\"" (* TODO Escaping *)
  | Symbol str -> str               (* TODO Escaping *)
  | Int int -> string_of_int int
  | Float float -> string_of_float float
  | Cons cons -> "(" ^ to_string_cons cons ^ ")"
and to_string_cons cons =
  match cons with
  | { car; cdr = Nil      } -> to_string car
  | { car; cdr = Cons cdr } -> to_string car ^ " " ^ to_string_cons cdr
  | { car; cdr = cdr      } -> to_string car ^ " . " ^ to_string cdr

module Reader = struct

  let integer =
    let re = Pcre.regexp "[-+]?[0-9]+"
    in (fun x -> int_of_string (get_substring x 0)) <@@> Parser.regexp re

  let float =
    let re = Pcre.regexp "[-+]?[0-9]+(\\.[0-9]+)([eE][-+]?[0-9]+)?"
    in (fun x -> float_of_string (get_substring x 0)) <@@> Parser.regexp re

  let string =
    let constituent =
      (char '\\' *> any_char) <|>
        (not_char '"')
    in char '"' *> (String.implode <@@> many constituent) <* char '"'

  let symbol =
    let constituent =
      (char '\\' *> any_char) <|>
        (satisfy (function
             | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') -> true
             | _ -> false))
    in String.implode <@@> some constituent

  let rec _list () =
    let list_element = skip_whitespace *> _expr () <* skip_whitespace
      and dot_handler = _expr () >>= fun e -> begin
                            skip_whitespace *>
                              char '.' *>
                                skip_whitespace *>
                                  _expr () >>= fun e1 -> begin
                                skip_whitespace *>
                                  pure (fun x ->
                                      (cons_append x (cons e e1)))
                              end
                          end
    in let dot_handler_maybe = dot_handler <|>
                                 pure (fun x -> cons_append x Nil)
       in char '(' *>
            (many list_element <**> dot_handler_maybe) <* char ')'
  and _expr () =
    begin
      ((fun x -> Int x) <@@> integer) <|>
        ((fun x -> Float x) <@@> float) <|>
        ((fun x -> String x) <@@> string) <|>
        ((fun x -> Symbol x) <@@> symbol)
    end <||>
      _list

  let expr = _expr ()
  let list = _list ()

end
