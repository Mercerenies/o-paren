
open Extlib
open ExtString
open BatPervasives
open Pcre
open Parser

type cons = { car: sexpr; cdr: sexpr }
and fun_type = { parms: string list * string option; body: sexpr list }
and sexpr =
  | Nil
  | String of string
  | Symbol of string
  | Int of int
  | Float of float
  | Cons of cons
  | Function of fun_type

type t = sexpr

let cons x y = Cons { car = x; cdr = y }

let cons_append xs y = List.fold_right cons xs y

let make_function parms body = Function { parms; body }

let rec to_string x = match x with
  | Nil -> "()"
  | String str -> "\"" ^ str ^ "\"" (* TODO Escaping *)
  | Symbol str -> str               (* TODO Escaping *)
  | Int int -> string_of_int int
  | Float float -> string_of_float float
  | Cons cons -> "(" ^ to_string_cons cons ^ ")"
  | Function _ -> "<function>"
and to_string_cons cons =
  match cons with
  | { car; cdr = Nil      } -> to_string car
  | { car; cdr = Cons cdr } -> to_string car ^ " " ^ to_string_cons cdr
  | { car; cdr = cdr      } -> to_string car ^ " . " ^ to_string cdr

module Reader = struct

  open ParserAlternative
  open ParserMonad
  module PMU = Cat.MonadUtils(ParserMonad)
  open PMU

  let is_symbol_char ch = match ch with
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9') -> true
    | '_' | '-' | '+' | '*' | '/' -> true
    | _ -> false

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
        (satisfy is_symbol_char)
    in String.uppercase <@@> (String.implode <@@> some constituent)

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
      _quoted <||>
      _list
  and _quoted () =
    char '\'' *> _expr () >>=
      fun e -> pure (cons (Symbol "QUOTE") (cons e Nil))

  let expr = _expr ()
  let list = _list ()
  let quoted = _quoted ()

end

let read_expr str =
  let module PMU = Cat.MonadUtils(ParserMonad)
  in let open PMU in
     Option.map (fun (x, _) -> x) @@
       run_parser (skip_whitespace *> Reader.expr) str

let read_exprs str =
  let module PMU = Cat.MonadUtils(ParserMonad)
  in let open PMU in
     Option.map_default (fun (x, _) -> x) [] @@
       run_parser (many (skip_whitespace *> Reader.expr)) str

module SList = struct

  type 'a expr_list = 'a list * 'a

  let from_list_generic cons (list, dot) =
    List.fold_right cons list dot

  let from_list = from_list_generic cons

  let rec to_list expr = match expr with
    | Cons { car; cdr } ->
       Util.first (fun rest -> car :: rest) (to_list cdr)
    | x ->
       ([], x)

  let nil_term x = (x, Nil)

end
