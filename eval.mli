
open Result

val lookup_name : Sxp.t Env.t -> string -> Sxp.t eval_result

val call_function :
      Sxp.t Env.t -> Sxp.fun_type -> Sxp.t -> Sxp.t eval_result

val eval_seq : Sxp.t Env.t -> Sxp.t list -> Sxp.t eval_result

val eval_list : Sxp.t Env.t -> Sxp.t -> Sxp.t eval_result

val eval : Sxp.t Env.t -> Sxp.t -> Sxp.t eval_result

(*
val eval_each : Sxp.t Env.t -> Sxp.t list -> Sxp.t list eval_result

val eval_seq : Sxp.t Env.t -> Sxp.t list -> Sxp.t eval_result
 *)
