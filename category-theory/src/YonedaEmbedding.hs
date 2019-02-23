{-# LANGUAGE RankNTypes #-}
module YonedaEmbedding where
import Data.Functor.Contravariant

left :: (forall x. (a -> x) -> (b -> x)) -> b -> a
right :: (b -> a) -> (forall x. (a -> x) -> (b -> x))

left f = f id
right ba ax = ax . ba

-- Yoneda Lemma

isoLeft :: (Functor f) => (forall x. (a -> x) -> f x) -> f a
isoLeft nt = nt id

isoRight :: (Functor f) => f a -> (forall x. (a -> x) -> f x)
isoRight fa g = fmap g fa

-- Co-Yoneda

coYoLeft :: (Contravariant f) => (forall x. (x -> a) -> f x) -> f a
coYoLeft nt = nt id

coYoRight :: (Contravariant f) => f a -> (forall x. (x -> a) -> f x)
coYoRight fa g = contramap g fa

-- Challenges
--1.Express the co-Yoneda embedding in Haskell.
coLeft :: (forall x. (x -> a) -> (x -> b)) -> a -> b
coLeft nt = nt id

coRight :: (a -> b) -> (forall x. (x -> a) -> (x -> b))
coRight ab xa = ab . xa


