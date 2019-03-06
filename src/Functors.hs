module Functors where
import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Contravariant

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
    fmap f (Reader g) = Reader (f . g)

newtype MyOp r a = MyOp (a -> r)
instance Contravariant (MyOp r) where
    contramap f (MyOp g) = MyOp (g . f)

-- Challenge 1
data Pair a b = Pair a b

instance Bifunctor Pair where
  bimap f g (Pair x y) = Pair (f x) (g y)

-- Challenge 2
type Maybe' a = Either (Const () a) (Identity a)

mapF ::  Maybe' a -> Maybe a
mapF (Left (Const ())) = Nothing
mapF (Right y) = Just (runIdentity y)

mapB :: Maybe a -> Maybe' a
mapB Nothing = Left (Const ())
mapB (Just x) = Right (Identity x)

-- Challenge 3
data PreList a b = Nil | Cons a b

instance Bifunctor PreList where
  bimap f g Nil = Nil
  bimap f g (Cons a b) = Cons (f a) (g b)


-- Challenge 4
newtype K2 c a b = K2 c
newtype Fst a b = Fst a
newtype Snd a b = Snd b


instance Bifunctor (K2 c) where
  bimap f g (K2 c) = K2 c

instance Bifunctor Fst where
  bimap f g (Fst a) = Fst (f a)

instance Bifunctor Snd where
  bimap f g (Snd b) = Snd (g b)
