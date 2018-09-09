
type 'a eval_result =
  | Success of 'a
  | Error of string

val eval : Sxp.t Env.t -> Sxp.t -> Sxp.t eval_result
