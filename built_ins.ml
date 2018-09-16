
open Sxp
open Result

module LocalMap = Map.Make(String)

(* (lambda args body ...) *)
let def_lambda env xs =
  let rec is_symbol x = match x with
    | Symbol _ -> true
    | _ -> false
  and validate_arglist args = match SList.to_list args with
    | (args, Symbol _) when List.for_all is_symbol args -> true
    | _ -> false
  in match xs with
     | Cons { car=parms; cdr=body } when validate_arglist parms ->
        begin
          match SList.to_list body with
          | (body, Nil) ->
             Success (Function { parms=parms; body=body })
          | _ -> Error "malformed lambda body"
        end
     | _ -> Error "malformed lambda expression"

let built_ins = [("LAMBDA", def_lambda)]

let built_in_map =
  let f acc kv = match kv with | (k, v) -> LocalMap.add k v acc
  in List.fold_left f LocalMap.empty built_ins

let construct_env () = new Env.env built_in_map
