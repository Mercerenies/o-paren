
type 'a eval_result =
  | Success of 'a
  | Error of string

val eval : Sxp.t Env.t -> Sxp.t -> Sxp.t eval_result
(*
val eval_each : Sxp.t Env.t -> Sxp.t list -> Sxp.t list eval_result

val eval_seq : Sxp.t Env.t -> Sxp.t list -> Sxp.t eval_result
 *)
