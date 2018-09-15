
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
and eval_seq env exprs = match exprs with
  | [] -> Success Nil
  | e :: [] -> eval env e
  | e :: es -> match eval env e with
               | Success _ -> eval_seq env es
               | Error x -> Error x
and eval (env : Sxp.t Env.t) expr =
  let rec _eval_args args0 = match args0 with
    | [] -> Success []
    | (y :: ys) -> match eval env y with
                   | Error e -> Error e
                   | Success z ->
                      match _eval_args ys with
                      | Error e -> Error e
                      | Success zs -> Success (z :: zs)
  and function_call x = match x with
    | (y :: ys, Nil) -> begin
        match eval env y with
        | Success f -> begin
            match _eval_args ys with
            | Success args -> begin
                match f with
                | Function f -> call_function env f args
                | _ -> Error "not a function"
              end
            | Error e -> Error e
          end
        | Error e -> Error e
      end
    | _ -> Error "malformed function call"
  in match expr with
     | Symbol s -> lookup_name env s
     | Cons _ -> function_call (SList.to_list expr)
     | x -> Success x
