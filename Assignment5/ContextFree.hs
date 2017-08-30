module ContextFree where

data Cat = S | NP | VP | V | D | N | PP | P deriving (Eq,Show)

data StrucDesc  = Binary Cat StrucDesc StrucDesc
                | Unary Cat StrucDesc
                | Leaf Cat String
                deriving (Show,Eq)

data GrammarRule    = BinaryStep Cat Cat Cat
                    | UnaryStep Cat Cat
                    | End Cat String
                    deriving (Show,Eq)

grammar1 =  [   -- S rules
                BinaryStep S NP VP,
                -- VP rules
                BinaryStep VP V NP,
                BinaryStep VP V S,
                BinaryStep VP VP PP,
                UnaryStep  VP V,
                -- PP rules
                BinaryStep PP P NP,
                -- D rules
                End D "the",
                End D "a",
                -- N rules
                End N "cat",
                End N "dog",
                -- NP rules
                BinaryStep NP D N,
                End NP "John",
                End NP "Mary",
                -- V rules
                End V "left",
                End V "thinks",
                -- P rules
                End P "with"
            ]

sd1 = Binary S (Leaf NP "John") (Binary VP (Leaf V "left") (Leaf NP "Mary"))

sd2 = Binary S (Leaf NP "John") (Binary VP (Unary VP (Leaf V "left")) (Binary PP (Leaf P "with") (Binary NP (Leaf D "a") (Leaf N "cat"))))

sd3 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd1)

sd4 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd2)

pf :: StrucDesc -> String
pf (Leaf c s) = s
pf (Unary c sd) = pf sd
pf (Binary c sd1 sd2) = pf sd1 ++ " " ++ pf sd2

categoryOf :: StrucDesc -> Cat
categoryOf (Leaf c s) = c
categoryOf (Unary c sd) = c
categoryOf (Binary c sd1 sd2) = c

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Leaf c s) = elem (End c s) g
wellFormed g (Unary c sd) = elem c (predecessorsUnary g (categoryOf sd)) && wellFormed g sd
wellFormed g (Binary c sd1 sd2) =
    elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2))
    && wellFormed g sd1 && wellFormed g sd2

predecessorsUnary :: [GrammarRule] -> Cat -> [Cat]
predecessorsUnary [] cat = []
predecessorsUnary (r:rs) cat =
    predecessorsUnary rs cat ++
        case r of
        UnaryStep parent child -> if child == cat then [parent] else []
        BinaryStep p ch1 ch2 -> []
        End c s -> []

predecessorsBinary :: [GrammarRule] -> (Cat,Cat) -> [Cat]
predecessorsBinary [] (cat1,cat2) = []
predecessorsBinary (r:rs) (cat1,cat2) =
    predecessorsBinary rs (cat1,cat2) ++
        case r of
        BinaryStep parent ch1 ch2 -> if (ch1,ch2) == (cat1,cat2) then [parent] else []
        UnaryStep parent ch -> []
        End c s -> []

numUnaries :: StrucDesc -> Int
numUnaries (Leaf c s) = 0
numUnaries (Unary c sd) = 1 + numUnaries sd
numUnaries (Binary c sd1 sd2) = numUnaries sd1 + numUnaries sd2

depth :: StrucDesc -> Int
depth (Leaf c s) = 1
depth (Unary c sd) = 1 + depth sd
depth (Binary c sd1 sd2) =
    let d1 = depth sd1 in
    let d2 = depth sd2 in
    1 + (if d1 > d2 then d1 else d2)

type Address = [Int]

-- Various cases where the result is undefined
-- (1) Something other than {0,1} at a Binary node
-- (2) Something other than 0 at a Unary node
-- (3) Something (anything at all!) at a Leaf node
getSubtree :: StrucDesc -> Address -> StrucDesc
getSubtree sd [] = sd
getSubtree (Leaf c s) (n:ns) = undefined
getSubtree (Unary c sd) (n:ns) =
    if n == 0 then (getSubtree sd ns) else undefined
    {- OR:  case n of {0 -> getSubtree sd ns; x -> undefined} -}
getSubtree (Binary c sd1 sd2) (n:ns) =
    case n of {0 -> getSubtree sd1 ns; 1 -> getSubtree sd2 ns; x -> undefined}

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.

brackets :: StrucDesc -> String
brackets (Leaf c s) = s
brackets (Unary c sd) = "[" ++ pf sd ++ "]"
brackets (Binary c sd1 sd2) = "[" ++ (brackets sd1) ++ " " ++ (brackets sd2) ++ "]"


labeledBrackets :: StrucDesc -> String
labeledBrackets (Leaf c s) = "[" ++ show c ++ " " ++ s ++ "]"
labeledBrackets (Unary c sd) = "[" ++ show c ++ " " ++ labeledBrackets sd ++ "]"
labeledBrackets (Binary c sd1 sd2) = "[" ++ show c ++ " " ++ (labeledBrackets sd1 ) ++ " " ++ (labeledBrackets sd2) ++ "]"

leftmostWord :: StrucDesc -> String
leftmostWord (Leaf c s) = s
leftmostWord (Unary c sd) = leftmostWord sd
leftmostWord (Binary c sd1 sd2) = leftmostWord sd1

numNPs :: StrucDesc -> Int
numNPs (Leaf c s) = if c == NP then 1 else 0
numNPs (Unary c sd) = if c == NP then 1 + numNPs sd else numNPs sd
numNPs (Binary c sd1 sd2) = if c == NP then 1 + numNPs sd1 + numNPs sd2 else numNPs sd1 + numNPs sd2

numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations g (Leaf c s) = if wellFormed g (Leaf c s) then 0 else 1
numViolations g (Unary c sd) = if wellFormed g (Unary c sd) then numViolations g sd 
                                else (1 + numViolations g sd)
numViolations g (Binary c sd1 sd2) = if (elem (BinaryStep c (categoryOf sd1) (categoryOf sd2)) g)
                                         then (numViolations g sd1 + numViolations g sd2)
                                        else (1 + numViolations g sd1 + numViolations g sd2)


sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap f (Leaf c s) = Leaf c (f s)
sdMap f (Unary c sd) = Unary c (sdMap f sd)
sdMap f (Binary c sd1 sd2) = Binary c (sdMap f sd1) (sdMap f sd2)

longestPath :: StrucDesc -> [Cat]
longestPath (Leaf c s) = [c]
longestPath (Unary c sd) = [c] ++ longestPath sd
longestPath (Binary c sd1 sd2) =
    if (length (longestPath sd1) >= length (longestPath sd2)) then
        [c] ++ longestPath sd1
    else [c] ++ longestPath sd2

allPaths :: StrucDesc -> [[Cat]]
allPaths (Leaf c s) = [[c]]
allPaths (Unary c sd) = map (\r -> c : r)(allPaths sd)
allPaths (Binary c sd1 sd2) = (map(\r -> c : r)(allPaths sd1)) ++ (map(\r -> c : r)(allPaths sd2))

addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs (Leaf c s) = if c == NP then [[]] else []
addressesOfNPs (Unary c sd) = if c == NP then [[]] ++ map (\r -> 0:r) (addressesOfNPs sd)
                                else map(\r -> 0:r)(addressesOfNPs sd)
addressesOfNPs (Binary c sd1 sd2) = if c == NP then [[]] ++ (map (\r -> 0:r) (addressesOfNPs sd1)) ++ (map (\r -> 1:r) (addressesOfNPs sd2))
                                        else (map (\r -> 0:r) (addressesOfNPs sd1)) ++ (map (\r -> 1:r) (addressesOfNPs sd2))

replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace sd [] sdr = sdr
replace (Leaf c s) (n:ns) sdr = undefined
replace (Unary c sd) (n:ns) sdr =
    if n == 0 then Unary c (replace sd ns sdr) else undefined
replace (Binary c sd1 sd2) (n:ns) sdr =
    case n of 
        0 -> (Binary c (replace sd1 ns sdr) sd2)
        1 -> (Binary c sd1 (replace sd2 ns sdr))
        x -> undefined


