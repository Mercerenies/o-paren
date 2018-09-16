
open Sxp
open Extlib
open Result

open ResultMonad
module RMU = Cat.MonadUtils(ResultMonad)
open RMU

let lookup_name env s =
  result_of_option "no such variable" (env#get_var s)

let rec bind_args env parms args = match (parms, args) with
  | (Symbol s, args) ->
     env#define_var s args;
     Success ()
  | (Cons { car=Symbol pcar; cdr=pcdr }, Cons { car=acar; cdr=acdr }) ->
     env#define_var pcar acar;
     bind_args env pcdr acdr
  | (Cons _, Nil) ->
     Error "not enough arguments"
  | (Cons _, _) ->
     Error "malformed argument list"
  | (Nil, Cons _) ->
     Error "too many arguments"
  | (Nil, Nil) ->
     Success ()
  | _ ->
     Error "lambda error"

let rec call_function env fn args =
  let { parms ; body } = fn
  in bind_args env parms args >>= fun _ -> eval_seq env body
and _eval_acc env exprs tail =
  let f acc x = acc >>= fun xs -> fmap (fun x -> x :: xs) (eval env x)
  in List.fold_left f (pure tail) exprs
and eval_seq env exprs = List.hd <@@> _eval_acc env exprs [Nil]
and eval_list env args = match args with
  | Cons { car; cdr } -> eval env car >>= (fun car ->
      eval_list env cdr >>= (fun cdr ->
        pure (Cons { car=car; cdr=cdr })))
  | x -> eval env x
and eval env expr =
  let rec function_call f xs =
    eval env f >>= (fun f ->
      match f with
      | Function f ->
         eval_list env xs >>= (fun args ->
          call_function env f args)
      | _ -> Error "not a function")
  in match expr with
     | Symbol s -> lookup_name env s
     | Cons { car=(Symbol s as car); cdr } -> begin
         match env#get_built_in s with
         | Some value -> value env cdr
         | None -> function_call car cdr
       end
     | Cons { car; cdr } -> function_call car cdr
     | x -> Success x
