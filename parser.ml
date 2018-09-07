
open Extlib

type 'a parser = string -> int -> ('a * int) option

let run_parser ff str = ff str 0

let map f gg = fun s n -> Option.map (Util.first f) (gg s n)

let pure x = fun _ _ -> Some (x, 0)

let (>>=) gg f = fun s n ->
  let inner (x, n1) = f x s n1
  in Util.join_option (Option.map inner (gg s n))
