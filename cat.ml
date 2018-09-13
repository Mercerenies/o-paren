
module type FUNCTOR = sig
  type 'a f
  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module type APPLICATIVE = sig
  include FUNCTOR
  val pure : 'a f
  val (<*>) : ('a -> 'b) f -> 'a f -> 'b f
end

module type MONAD = sig
  include APPLICATIVE
  val (>>=) : 'a f -> ('a -> 'b f) -> 'b f
end

module FunctorUtils (F : FUNCTOR) = struct
  open F
  type 'a f = 'a F.f
  let map = fmap
  let (<@@>) = fmap
end

module ApplicativeUtils (F : APPLICATIVE) = struct
  open F
  module MyFunctor = FunctorUtils(F)
  open MyFunctor
  let ( <* ) a b = (fun a _ -> a) <@@> a <*> b
  let ( *> ) a b = (fun _ b -> b) <@@> a <*> b
  let (<**>) a f = (fun a f -> f a) <@@> a <*> f
end

module MonadUtils (M : MONAD) = struct
end
