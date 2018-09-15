
open Extlib

type 'a eval_result =
  | Success of 'a
  | Error of string

module ResultFunctor = struct
  type 'a f = 'a eval_result
  let fmap f x = match x with
    | Error err -> Error err
    | Success z -> Success (f z)
end

module ResultApplicative = struct
  include ResultFunctor
  let pure x = Success x
  let (<*>) ff xx = match ff with
    | Error err -> Error err
    | Success f -> fmap f xx
end

module ResultMonad = struct
  include ResultApplicative
  let (>>=) xx f = match xx with
    | Error err -> Error err
    | Success x -> f x
end

let result_of_option s =
  Option.map_default (fun x -> Success x) (Error s)
