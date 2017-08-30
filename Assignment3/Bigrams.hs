module Bigrams where

-----------------------------------------------------------------
-- Old stuff and helpers

data Numb = Z | S Numb deriving (Show,Eq)

applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f list =
    case list of
    [] -> []
    (x:xs) -> (f x) : (applyToAll f xs)

-----------------------------------------------------------------
-- Data types for our bigram grammars

data GrammarRule = Step String String | End String deriving (Show,Eq)

data StrucDesc = Last String | NonLast String StrucDesc deriving (Show,Eq)

-----------------------------------------------------------------
-- Sample grammars and structural descriptions

grammar1 :: [GrammarRule]
grammar1 = [Step "the" "hamsters",
            Step "the" "small",
            Step "the" "very",
            Step "very" "small",
            Step "small" "hamsters",
            Step "hamsters" "run",
            Step "hamsters" "walk",
            Step "run" "quickly",
            Step "walk" "quickly",
            Step "walk" "slowly",
            End "run",
            End "quickly",
            End "slowly"
            ]

grammar2 :: [GrammarRule]
grammar2 = grammar1 ++ [Step "very" "very"]

sd1 :: StrucDesc
sd1 = NonLast "the" (NonLast "small" (NonLast "hamsters" (Last "run")))

sd2 :: StrucDesc
sd2 = NonLast "chipmunks" (Last "run")

sd3 :: StrucDesc
sd3 = NonLast "run" (Last "hamsters")

sd4 :: StrucDesc
sd4 = Last "quickly"

-----------------------------------------------------------------
-- Some provided functions

firstWord :: StrucDesc -> String
firstWord sd =
    case sd of
    Last s -> s
    NonLast s sd -> s

enders :: [GrammarRule] -> [String]
enders [] = []
enders (r:rest) =
    case r of
    End x -> (x : enders rest)
    Step x y -> enders rest

successors :: [GrammarRule] -> String -> [String]
successors [] s = []
successors (r:rest) s =
    case r of
    End x -> successors rest s
    Step x y -> if x == s then (y : successors rest s) else successors rest s

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Last s) = elem s (enders g)
wellFormed g (NonLast s sd) = if elem (firstWord sd) (successors g s) then wellFormed g sd else False

-- -- This is the version of wellFormed that we wrote in class on 1/25.
-- wellFormed :: [GrammarRule] -> StrucDesc -> Bool
-- wellFormed g sd =
--     case sd of
--     Last s -> elem (End s) g
--     NonLast s sd'->
--         -- elem (Step s (firstWord sd')) g && wellFormed g sd'
--         if elem (Step s (firstWord sd')) g then
--             wellFormed g sd'
--         else
--             False

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.

-----------------------------------------------------------------
-- 1. Working with structural descriptions

sdLength :: StrucDesc -> Numb
sdLength sd = 
    case sd of
    Last s -> S Z
    NonLast s sd' -> S (sdLength sd')


lastWord :: StrucDesc -> String
lastWord sd =
    case sd of
    Last s -> s
    NonLast s sd' -> lastWord sd'

pf :: StrucDesc -> String
pf sd =
    case sd of
    Last s -> s
    NonLast s sd' -> s ++ " " ++ pf sd'

prependWord :: StrucDesc -> String -> StrucDesc
prependWord sd s = NonLast s sd

-----------------------------------------------------------------
-- 2. Working with grammars

predecessors :: [GrammarRule] -> String -> [String]
predecessors g s =
    case g of
    [] -> []
    (r:rest) -> case r of
        End x -> predecessors rest s
        Step x y -> if y == s then (x : predecessors rest s) else predecessors rest s

rulesFromSentence :: [String] -> [GrammarRule]
rulesFromSentence list = 
    case list of
    [] -> []
    (x:[]) -> [End x]
    (x:(y:rest)) -> [Step x y] ++ rulesFromSentence (y:rest)

rulesFromText :: [String] -> [GrammarRule]
rulesFromText list = concat (map rulesFromSentence (map words list))

-----------------------------------------------------------------
-- 3. Putting it all together

extendByOne :: [GrammarRule] -> StrucDesc -> [StrucDesc]
extendByOne g sd = applyToAll (\s -> NonLast s (sd)) (predecessors g (firstWord sd))

extend :: [GrammarRule] -> Numb -> StrucDesc -> [StrucDesc]
extend g n sd =
    case n of
    Z -> [sd]
    S n' -> [sd] ++ concat (map (\s -> extend g n' s) (extendByOne g sd))

generate :: [GrammarRule] -> Numb -> [StrucDesc]
generate g n = 
    case n of
    Z -> []
    S n' -> concat (map (\s -> extend g n' s) (map (\s -> Last s) (enders g)))

threeSteps :: [GrammarRule] -> String -> [String]
threeSteps g s = 
    let oneStep = (successors g s) in
    let twoSteps = concat (map (\s' -> successors g s') oneStep) in
        -- concat (map (successors g) oneStep)
    concat (map (successors g) twoSteps)
    -- concatMap (successors g) twoSteps

