
open Extlib
open ExtString
open BatPervasives
open Pcre

type cons = { car: sexpr; cdr: sexpr }
and sexpr =
  | Nil
  | String of string
  | Symbol of string
  | Int of int
  | Float of float
  | Cons of cons

let cons x y = Cons { car = x; cdr = y }

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

  let skip_whitespace str =
    let w_re = regexp "^\\s*"
    in let matched = get_substring (exec ~rex:w_re str) 0
       in Util.drop_string (String.length matched) str

  let read_integer str =
    let int_re = regexp "^[-+]?[0-9]+"
    in try
      let matched = get_substring (exec ~rex:int_re str) 0
      in let len = String.length matched
         in Some (int_of_string matched, Util.drop_string len str)
    with Not_found -> None

  let read_float str =
    let float_re = regexp "^[-+]?[0-9]+(\\.[0-9]+)([eE][-+]?[0-9]+)?"
    in try
      let matched = get_substring (exec ~rex:float_re str) 0
      in let len = String.length matched
         in Some (float_of_string matched, Util.drop_string len str)
    with Not_found -> None

  let read_string str =
    let rec parse_string n =
      match Util.safe_get str n with
      | Some '\"' -> Some ([], Util.drop_string (n + 1) str)
      | Some '\\' -> begin
          match Util.safe_get str n with
          | None -> None
          | Some ch ->
             Option.map (Util.first (fun z -> ch :: z))
               (parse_string (n + 2))
        end
      | Some ch -> Option.map (Util.first (fun z -> ch :: z))
                     (parse_string (n + 1))
      | None -> None
    in match Util.safe_get str 0 with
    | Some '\"' -> Option.map (Util.first String.implode)
                     (parse_string 1)
    | _ -> None

  let read_symbol str =
    let rec parse_symbol n =
      match Util.safe_get str n with
      | Some '\\' -> begin
          match Util.safe_get str n with
          | None -> None
          | Some ch ->
             Option.map (Util.first (fun z -> ch :: z))
               (parse_symbol (n + 2))
        end
      | Some ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' as ch) ->
         Option.map (Util.first (fun z -> ch :: z))
           (parse_symbol (n + 1))
      | _ -> Some ([], Util.drop_string n str)
    in match parse_symbol 0 with
       | None -> None
       | Some ([], _) -> None
       | Some (x, str) -> Some (String.implode x, str)

  let rec read_list str =
    match Util.safe_get str 0 with
    | Some '(' ->
       read_list_inner (skip_whitespace (Util.drop_string 1 str))
    | _ -> None
  and read_list_inner str =
    match Util.safe_get str 0 with
    | Some '.' -> begin
        match read_expr (skip_whitespace (Util.drop_string 1 str)) with
        | None -> None
        | Some (x, str) -> begin
            let str = skip_whitespace str
            in match Util.safe_get str 0 with
               | Some ')' -> Some (x, Util.drop_string 1 str)
               | _ -> None
          end
      end
    | Some ')' -> Some (Nil, Util.drop_string 1 str)
    | _ -> begin
        match read_expr str with
        | None -> None
        | Some (x, str) -> Option.map (Util.first (cons x))
                             (read_list_inner (skip_whitespace str))
      end
  and read_expr str =
    let possible = [read_list str;
                    Option.map (Util.first (fun x -> String x))
                      (read_string str);
                    Option.map (Util.first (fun x -> Float x))
                      (read_float str);
                    Option.map (Util.first (fun x -> Int x))
                      (read_integer str);
                    Option.map (Util.first (fun x -> Symbol x))
                      (read_symbol str)]
    in List.fold_left Util.merge None possible

end
