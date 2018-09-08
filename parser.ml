
open Extlib
open ExtString
open Pcre

type 'a t = string -> int -> ('a * int) option

let run_parser ff str = ff str 0

let map f gg = fun s n -> Option.map (Util.first f) (gg s n)

let pure x = fun _ n -> Some (x, n)

let empty = fun _ _ -> None

let (>>=) gg f = fun s n ->
  let inner (x, n1) = f x s n1
  in Util.join_option (Option.map inner (gg s n))

let (<*>) ff xx = ff >>= fun (f) -> xx >>= fun (x) -> pure (f x)

let (<@@>) = map

let (<**>) xx ff = (fun x f -> f x) <@@> xx <*> ff

let (<|>) xx yy = fun s n -> Util.merge (xx s n) (yy s n)

let (<||>) xx y0 = fun s n -> match xx s n with
                              | Some x -> Some x
                              | None -> y0 () s n

let ( *> ) xx yy = (fun _ y -> y) <@@> xx <*> yy

let ( <* ) xx yy = (fun x _ -> x) <@@> xx <*> yy

let sequence x =
  List.fold_right (fun aa ss -> Util.cons <@@> aa <*> ss) x (pure [])

let regexp rex = fun s n ->
  try
    let ss = exec ~rex ~flags:[`ANCHORED] ~pos:n s
    in Some (ss, n + String.length (get_substring ss 0))
  with Not_found -> None

let skip_whitespace =
  map ignore (regexp (Pcre.regexp "\\s*"))

let satisfy p = fun s n ->
  match Util.safe_get s n with
  | Some c when p c -> Some (c, n + 1)
  | _ -> None

let char ch = satisfy ((==) ch)

let not_char ch = satisfy ((!=) ch)

let any_char = satisfy (fun _ -> true)

let string str = map String.implode @@
                   sequence @@
                     List.map char @@
                       String.explode str

let one_of x = List.fold_left (<|>) empty x

(* We'd like to simply write `Util.cons <@@> x <*> many x',
 * but in a strict language this would never terminate, so
 * we need to short circuit the parsing ourselves. Enter
 * monadic binds. *)
let rec some x = x >>= fun x0 -> many x >>= fun mx -> pure (x0 :: mx)
  and many x = some x <|> pure []

let optional x = ((fun x -> Some x) <@@> x) <|> pure None
