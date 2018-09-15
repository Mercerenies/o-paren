
open Cat

type 'a eval_result =
  | Success of 'a
  | Error of string

module ResultFunctor : FUNCTOR with type 'a f = 'a eval_result
module ResultApplicative : APPLICATIVE with type 'a f = 'a eval_result
module ResultMonad : MONAD with type 'a f = 'a eval_result

val result_of_option : string -> 'a option -> 'a eval_result
