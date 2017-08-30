module LoopDetection where

import Bigrams

type Position = ([String],String)
-- a position (ws,s) represents being at word w, and having there by
-- stepping through the ws order

-- return true iff using the grammar we can get into a loop starting from the given string
canLoop :: [GrammarRule] -> String -> Bool
canLoop g s = canLoop' g ([],s)

canLoop' :: [GrammarRule] -> Position -> Bool
canLoop' g (history,w) =
	--nextWords is all things we might move to next after w
	let nextWords = successors g w in
		if any (\nextWord -> elem nextWord history) nextWords then True
		-- if any of the nextWords are already in history, then True
		else
		-- otherwise, create Position representing having movedto each nextWord,
		-- and see if we can reach a loop from any of those.
		-- The call to "any"; will return false whenever nextWords is empty
		-- ie. whenever nextWords is empty
		
			let newPositions = map (\nextWord -> (history ++ [w], nextWord)) nextWords in
				any (canLoop' g) newPositions