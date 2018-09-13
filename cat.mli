
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

module type ALTERNATIVE = sig
  include APPLICATIVE
  val empty : 'a f
  val (<|>) : 'a f -> 'a f -> 'a f
end

module FunctorUtils (F : FUNCTOR) : sig
  val map : ('a -> 'b) -> 'a F.f -> 'b F.f
  val (<@@>) : ('a -> 'b) -> 'a F.f -> 'b F.f
end

module ApplicativeUtils (F : APPLICATIVE) : sig
  include module type of struct include FunctorUtils(F) end
  val ( <* ) : 'a F.f -> 'b F.f -> 'a F.f
  val ( *> ) : 'a F.f -> 'b F.f -> 'b F.f
  val (<**>) : 'a F.f -> ('a -> 'b) F.f -> 'b F.f
end

module MonadUtils (F : MONAD) : sig
  include module type of struct include ApplicativeUtils(F) end
  val return : 'a -> 'a F.f
  val join : 'a F.f F.f -> 'a F.f
  val sequence : 'a F.f list -> 'a list F.f
end

module AlternativeUtils (F : ALTERNATIVE) : sig
  include module type of struct include ApplicativeUtils(F) end
end

module ListFunctor : FUNCTOR with type 'a f = 'a list
module ListApplicative : APPLICATIVE with type 'a f = 'a list
module ListMonad : MONAD with type 'a f = 'a list
module ListAlternative : ALTERNATIVE with type 'a f = 'a list

module OptionFunctor : FUNCTOR with type 'a f = 'a option
module OptionApplicative : APPLICATIVE with type 'a f = 'a option
module OptionMonad : MONAD with type 'a f = 'a option
module OptionAlternative : ALTERNATIVE with type 'a f = 'a option
