
type 'a scope_frame = 'a Map.Make(String).t

class ['a] env (built_ins : ('a env -> unit) Map.Make(String).t) =
object (self)
  val mutable scope = ([] : 'a scope_frame list)
end

type 'a t = 'a env
