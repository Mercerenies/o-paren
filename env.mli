
type 'a scope_frame = 'a Map.Make(String).t

class ['a] env : ('a env -> 'a -> 'a) Map.Make(String).t -> object

  method define_var : string -> 'a -> unit

  (* Returns whether successful *)
  method set_var : string -> 'a -> bool

  method get_var : string -> 'a option

  method get_built_in : string -> ('a env -> 'a -> 'a) option

  method push_scope : unit -> unit

  method pop_scope : unit -> 'a scope_frame option

  method in_scope : 'b. (unit -> 'b) -> 'b

end

type 'a t = 'a env
