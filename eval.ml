
open Sxp
open Extlib

type 'a eval_result =
  | Success of 'a
  | Error of string

let eval (env : Sxp.t Env.t) expr =
  let rec lookup_name s =
    Option.map_default (fun x -> Success x) (Error "no such variable")
      (env#get_var s)
  and _eval_args args0 = match args0 with
    | [] -> Success []
    | (y :: ys) -> match _eval y with
                   | Error e -> Error e
                   | Success z ->
                      match _eval_args ys with
                      | Error e -> Error e
                      | Success zs -> Success (z :: zs)
  and perform_call { args = parms ; body = body } args =
    env#in_scope begin
        fun () ->
        try
          List.iter2 env#define_var parms args;
          _eval_body body
        with Invalid_argument _ -> Error "wrong number of arguments"
      end
  and function_call x = match x with
    | (y :: ys, Nil) -> begin
        match _eval y with
        | Success f -> begin
            match _eval_args ys with
            | Success args -> begin
                match f with
                | Function f -> perform_call f args
                | _ -> Error "not a function"
              end
            | Error e -> Error e
          end
        | Error e -> Error e
      end
    | _ -> Error "malformed function call"
  and _eval expr = match expr with
    | Symbol s -> lookup_name s
    | Cons _ -> function_call (SList.to_list expr)
    | x -> Success x
  and _eval_body exprs = match exprs with
    | [] -> Success Nil
    | e :: [] -> _eval e
    | e :: es -> match _eval e with
                 | Success _ -> _eval_body es
                 | Error x -> Error x
  in _eval expr
