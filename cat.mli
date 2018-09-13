
module type FUNCTOR = sig
  type 'a f
  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module type APPLICATIVE = sig
  include FUNCTOR
  val pure : 'a -> 'a f
  val (<*>) : ('a -> 'b) f -> 'a f -> 'b f
end

module type MONAD = sig
  include APPLICATIVE
  val (>>=) : 'a f -> ('a -> 'b f) -> 'b f
end

module FunctorUtils (F : FUNCTOR) : sig
  val map : ('a -> 'b) -> 'a F.f -> 'b F.f
  val (<@@>) : ('a -> 'b) -> 'a F.f -> 'b F.f
end

module ApplicativeUtils (F : APPLICATIVE) : sig
  val ( <* ) : 'a F.f -> 'b F.f -> 'a F.f
  val ( *> ) : 'a F.f -> 'b F.f -> 'b F.f
  val (<**>) : 'a F.f -> ('a -> 'b) F.f -> 'b F.f
end

module MonadUtils (M : MONAD) : sig
  val return : 'a -> 'a M.f
end

module ListFunctor : FUNCTOR with type 'a f = 'a list
module ListApplicative : APPLICATIVE with type 'a f = 'a list
module ListMonad : MONAD with type 'a f = 'a list
