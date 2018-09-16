
open Sxp
open Result

module LocalMap = Map.Make(String)

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
        try
          let parms = validate_arglist parms
          in match SList.to_list body with
             | (body, Nil) ->
                Success (Function { parms=parms; body=body })
             | _ -> Error "malformed lambda body"
        with Failure _ -> Error "malformed lambda expression"
     | _ -> Error "malformed lambda expression"

let built_ins = [("LAMBDA", def_lambda)]

let built_in_map =
  let f acc kv = match kv with | (k, v) -> LocalMap.add k v acc
  in List.fold_left f LocalMap.empty built_ins

let construct_env () = new Env.env built_in_map
