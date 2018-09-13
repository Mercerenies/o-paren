
open Cat

type 'a eval_result =
  | Success of 'a
  | Error of string

module ResultFunctor : FUNCTOR with type 'a f = 'a eval_result
module ResultApplicative : APPLICATIVE with type 'a f = 'a eval_result
module ResultMonad : MONAD with type 'a f = 'a eval_result

val eval : Sxp.t Env.t -> Sxp.t -> Sxp.t eval_result
(*
val eval_each : Sxp.t Env.t -> Sxp.t list -> Sxp.t list eval_result

val eval_seq : Sxp.t Env.t -> Sxp.t list -> Sxp.t eval_result
 *)
