module NaturalTransformations where
import Functors


obvious :: Reader () a -> Maybe a
obvious (Reader g) = Just (g ())

-- Challenge 1
toList ::  Maybe a -> [a]
toList Nothing = []
toList (Just x) = [x]

-- Challenge 2
readerToList1 :: Reader () a -> [a]
readerToList1 (Reader _) = []

readerToList2 :: Reader () a -> [a]
readerToList2 (Reader f) = [f ()]

readerToList3 :: Reader () a -> [a]
readerToList3 (Reader f) = [f (), f ()]

-- Challenge 3
readerBoolToMaybe :: Reader Bool a -> Maybe a
readerBoolToMaybe (Reader _) = Nothing

readerBoolToMaybe1 :: Reader Bool a -> Maybe a
readerBoolToMaybe1 (Reader f) = Just (f True)

readerBoolToMaybe2 :: Reader Bool a -> Maybe a
readerBoolToMaybe2 (Reader f) = Just (f False)
