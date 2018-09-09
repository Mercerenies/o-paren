
type 'a scope_frame = 'a Map.Make(String).t

class ['a] env : ('a env -> unit) Map.Make(String).t -> object
  val scope : 'a scope_frame list
end

type 'a t = 'a env
