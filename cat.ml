
open Extlib

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

module FunctorUtils (F : FUNCTOR) = struct
  open F
  type 'a f = 'a F.f
  let map = fmap
  let (<@@>) = fmap
end

module ApplicativeUtils (F : APPLICATIVE) = struct
  open F
  module MyFunctor = FunctorUtils(F)
  include MyFunctor
  let ( <* ) a b = (fun a _ -> a) <@@> a <*> b
  let ( *> ) a b = (fun _ b -> b) <@@> a <*> b
  let (<**>) a f = (fun a f -> f a) <@@> a <*> f
end

module MonadUtils (F : MONAD) = struct
  open F
  module MyApplicative = ApplicativeUtils(F)
  include MyApplicative
  let return = pure
  let join x = x >>= fun x -> x
  let sequence x =
    List.fold_right (fun aa ss -> Util.cons <@@> aa <*> ss) x (pure [])
end

module AlternativeUtils (F : ALTERNATIVE) = struct
  module MyApplicative = ApplicativeUtils(F)
  include MyApplicative
end

module ListFunctor = struct
  type 'a f = 'a list
  let fmap = List.map
end

module ListApplicative = struct
  include ListFunctor
  let pure x = [x]
  let (<*>) ff xx = List.concat (fmap (fun f -> fmap f xx) ff)
end

module ListMonad = struct
  include ListApplicative
  let (>>=) xx f = List.concat (fmap f xx)
end

module ListAlternative = struct
  include ListApplicative
  let empty = []
  let (<|>) = List.append
end

module OptionFunctor = struct
  type 'a f = 'a option
  let fmap = Option.map
end

module OptionApplicative = struct
  include OptionFunctor
  let pure x = Some x
  let (<*>) ff xx = match ff with
    | None -> None
    | Some f -> fmap f xx
end

module OptionMonad = struct
  include OptionApplicative
  let (>>=) xx f = match xx with
    | None -> None
    | Some x -> f x
end

module OptionAlternative = struct
  include OptionApplicative
  let empty = None
  let (<|>) x y = match x with
    | None -> y
    | _    -> x
end
