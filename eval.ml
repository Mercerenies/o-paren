
open Sxp
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

open ResultMonad
module RMU = Cat.MonadUtils(ResultMonad)
open RMU

let result_of_option s =
  Option.map_default (fun x -> Success x) (Error s)

let lookup_name env s =
  result_of_option "no such variable" (env#get_var s)

let rec call_function env fn args =
  let { parms ; body } = fn
  in try
    List.iter2 env#define_var parms args;
    eval_seq env body
  with Invalid_argument _ -> Error "wrong number of arguments"
and _eval_acc env exprs tail =
  let f acc x = acc >>= fun xs -> fmap (fun x -> x :: xs) (eval env x)
  in List.fold_left f (pure tail) exprs
and eval_seq env exprs = List.hd <@@> _eval_acc env exprs [Nil]
and eval_list env args = List.rev <@@> _eval_acc env args []
and eval (env : Sxp.t Env.t) expr =
  let rec function_call f xs = match xs with
    | (xs, Nil) ->
       eval env f >>= (fun f ->
        match f with
        | Function f ->
           eval_list env xs >>= (fun args ->
            call_function env f args)
        | _ -> Error "not a function")
    | _ -> Error "malformed function call"
  in match expr with
     | Symbol s -> lookup_name env s
     | Cons { car; cdr } -> function_call car (SList.to_list cdr)
     | x -> Success x
