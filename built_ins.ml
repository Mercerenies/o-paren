
open Sxp
open Result

module LocalMap = Map.Make(String)

open ResultMonad

(* (lambda args body ...) *)
let def_lambda env xs =
  let rec string_of_symbol x = match x with
    | Symbol x -> x
    | _ -> failwith "string_of_symbol"
  and validate_arglist args = match SList.to_list args with
    | (args, Symbol s) -> (List.map string_of_symbol args, Some s)
    | (args, Nil) -> (List.map string_of_symbol args, None)
    | _ -> failwith "validate_arglist"
  in match xs with
     | Cons { car=parms; cdr=body } ->
        begin
          try
            let parms = validate_arglist parms
            in match SList.to_list body with
               | (body, Nil) ->
                  Success (Function { parms=parms; body=body })
               | _ -> Error "malformed lambda body"
          with Failure _ -> Error "malformed lambda expression"
        end
   | _ -> Error "malformed lambda expression"

(* (quote x) *)
let def_quote env xs =
  match SList.to_list xs with
  | ([x], Nil) -> Success x
  | _ -> Error "malformed quote expression"

(* (define var body ...)
   (define (f args ...) body ...) *)
let def_define env xs =
  let go s body =
    match SList.to_list body with
    | (body, Nil) ->
       fmap (fun x -> env#define_var s x; x)
         (Eval.eval_seq env body)
    | _ -> Error "malformed define statement"
  in match xs with
     | Cons {car=Symbol s; cdr=body} ->
        go s body
     | Cons {car=Cons { car=Symbol s; cdr=args }; cdr=body} ->
        def_lambda env (Cons { car=args; cdr=body }) >>= go s
     | _ -> Error "malformed define statement"

(* (set a b) *)
let def_set env xs =
  match SList.to_list xs with
  | ([a; b], Nil) ->
     Eval.eval env a >>= (fun a ->
      match a with
      | Symbol s -> Eval.eval env b >>= fun b ->
                    if env#set_var s b then
                      pure b
                    else
                      Error "no such variable"
      | _ -> Error "not a symbol")
  | _ -> Error "malformed set statement"

let def_setq env xs =
  match SList.to_list xs with
  | ([Symbol s; b], Nil) ->
     Eval.eval env b >>= fun b ->
     if env#set_var s b then
       pure b
     else
       Error "no such variable"
  | _ -> Error "malformed setq statement"

let built_ins : (string * Sxp.t Env.builtin) list =
  [("LAMBDA", def_lambda);
   ("QUOTE", def_quote);
   ("DEFINE", def_define);
   ("SET", def_set);
   ("SETQ", def_setq)]

let built_in_map =
  let f acc kv = match kv with | (k, v) -> LocalMap.add k v acc
  in List.fold_left f LocalMap.empty built_ins

let construct_env () = new Env.env built_in_map
