
open Sxp
open Extlib
open Result

open ResultMonad
module RMU = Cat.MonadUtils(ResultMonad)
open RMU

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
and eval env expr =
  let rec function_call f xs = match xs with
    | (xs, Nil) ->
       eval env f >>= (fun f ->
        match f with
        | Function f ->
           eval_list env xs >>= (fun args ->
            call_function env f args)
        | _ -> Error "not a function")
    | _ -> Error "malformed function call"
  and builtin_call f xs = match xs with
    | (xs, Nil) -> f env xs
    | _ -> Error "malformed built-in call"
  in match expr with
     | Symbol s -> lookup_name env s
     | Cons { car=(Symbol s as car); cdr } -> begin
         match env#get_built_in s with
         | Some value -> builtin_call value (SList.to_list cdr)
         | None -> function_call car (SList.to_list cdr)
       end
     | Cons { car; cdr } -> function_call car (SList.to_list cdr)
     | x -> Success x
