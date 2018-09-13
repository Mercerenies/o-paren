
open Extlib
open ExtString
open Pcre
open Cat

type 'a t = string -> int -> ('a * int) option

let run_parser ff str = ff str 0

module ParserFunctor = struct
  type 'a f = 'a t
  let fmap f gg = fun s n -> Option.map (Util.first f) (gg s n)
end

module ParserApplicative = struct
  include ParserFunctor
  let pure x = fun _ n -> Some (x, n)
  let (<*>) ff xx = fun s n ->
    match ff s n with
    | None -> None
    | Some (f, n1) -> Option.map (Util.first f) (xx s n1)
end

module ParserMonad = struct
  include ParserApplicative
  module OMU = Cat.MonadUtils(Cat.OptionMonad)
  let (>>=) gg f = fun s n ->
    let inner (x, n1) = f x s n1
    in OMU.join (Option.map inner (gg s n))
end

open ParserMonad
module PMU = MonadUtils(ParserMonad)
open PMU

let empty = fun _ _ -> None

let (<**>) xx ff = (fun x f -> f x) <@@> xx <*> ff

let (<|>) xx yy = fun s n -> Util.merge (xx s n) (yy s n)

let (<||>) xx y0 = fun s n -> match xx s n with
                              | Some x -> Some x
                              | None -> y0 () s n

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
