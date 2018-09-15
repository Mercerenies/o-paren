
open Result

type 'a scope_frame = 'a Map.Make(String).t

class ['b, 'a] env : 'b Map.Make(String).t -> object

  method define_var : string -> 'a -> unit

  (* Returns whether successful *)
  method set_var : string -> 'a -> bool

  method get_var : string -> 'a option

  method get_built_in : string -> 'b option

  method push_scope : unit -> unit

  method pop_scope : unit -> 'a scope_frame option

  method in_scope : 't. (unit -> 't) -> 't

end

type 'a builtin = ('a builtin, 'a) env -> 'a list -> 'a eval_result

type 'a t = ('a builtin, 'a) env
