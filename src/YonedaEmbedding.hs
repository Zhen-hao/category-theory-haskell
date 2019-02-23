module YonedaEmbedding where

left :: (forall x. (a -> x) -> (b -> x)) -> b -> a
right :: (b -> a) -> (forall x. (a -> x) -> (b -> x))


left f = f id
right ba ax b = ax . ba b
