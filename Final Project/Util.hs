module Util where

import qualified Data.Map as Map

------------------------------------------------------------

-- Feel free to ignore the implementation of this function
printMap :: (Show k, Show v) => Map.Map k v -> IO ()
printMap m = (putStr . unlines) (map show (Map.toList m))

------------------------------------------------------------

-- We've seen this one before
sumOver :: (a -> Double) -> [a] -> Double
sumOver f xs = sum (map f xs)

maxOver :: (a -> Double) -> [a] -> (Double,a)
maxOver f [] = undefined
maxOver f list =
	case list of
		(a:[]) -> (f a,a)
		(a1:(a2:r)) -> if (f a1) < (f a2) then maxOver f (a2:r)
							else maxOver f (a1:r)

------------------------------------------------------------

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] [b] = []
allPairs a_list b_list =
	case a_list of
		[] -> []
		(a1:[]) -> elemToPair a1 b_list
		(a1:(a2:rest)) -> elemToPair a1 b_list ++ allPairs (a2:rest) b_list

elemToPair :: a -> [b] -> [(a,b)]
elemToPair a [] = []
elemToPair a b_list = 
	case b_list of
		[] -> []
		(b1:[]) -> [(a,b1)]
		(b1:(b2:rest)) -> [(a,b1)] ++ elemToPair a (b2:rest)
------------------------------------------------------------

updateForAll :: (t -> c -> t) -> t -> [c] -> t
updateForAll f t c_list =
	case c_list of
		[] -> undefined
		(c1:[]) -> f t c1
		(c1:(c2:rest)) -> updateForAll f (f t c1) (c2:rest)

