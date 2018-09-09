
module ScopeMap = Map.Make(String)

type 'a scope_frame = 'a Map.Make(String).t

class ['a] env (built_ins : ('a env -> 'a) Map.Make(String).t) =
object (self)
  val mutable scope =
    ((ScopeMap.empty, []) : 'a scope_frame * 'a scope_frame list)

  method define_var s x =
    let (a, b) = scope in
    scope <- (ScopeMap.add s x a, b)

  (* Returns whether successful *)
  method set_var s x =
    let rec _set ys = match ys with
      | (y, ys) when ScopeMap.mem s y -> (true, ScopeMap.add s x y :: ys)
      | (y, ys) -> Util.second (fun ys -> y :: ys) (_set_go ys)
    and _set_go ys = match ys with
      | y :: ys -> _set (y, ys)
      | [] -> (false, [])
    in let (b, y :: ys) = _set scope in
       scope <- (y, ys);
       b

  method get_var s =
    let rec _get ys = match ys with
      | [] -> None
      | y :: ys ->
         try
           Some (ScopeMap.find s y)
         with Not_found -> _get ys
    in let (y, ys) = scope
       in _get (y :: ys)

  method push_scope () =
    let (a, b) = scope
    in scope <- (ScopeMap.empty, a :: b)

  method pop_scope () =
    match scope with
    | (_, []) -> None
    | (a, y :: ys) ->
       scope <- (y, ys);
       Some a

end

type 'a t = 'a env
