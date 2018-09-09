
val built_in_map : (Sxp.t Env.t -> Sxp.t -> Sxp.t) Map.Make(String).t

val construct_env : unit -> Sxp.t Env.t
