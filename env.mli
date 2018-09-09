
type 'a scope_frame = 'a Map.Make(String).t

class ['a] env : ('a env -> 'a) Map.Make(String).t -> object
  val scope : 'a scope_frame * 'a scope_frame list

  method define_var : string -> 'a -> unit

  (* Returns whether successful *)
  method set_var : string -> 'a -> bool

  method get_var : string -> 'a option

  method push_scope : unit -> unit

  method pop_scope : unit -> 'a scope_frame option

end

type 'a t = 'a env
