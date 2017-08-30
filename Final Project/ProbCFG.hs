module ProbCFG where

import qualified Util
import qualified Data.Map as Map

data Cat = S | NP | VP | N | D | V | PP | P | Adv deriving (Eq,Show,Ord)

data StrucDesc = Leaf Cat String | Binary Cat StrucDesc StrucDesc
                 deriving (Eq,Show)

type ProbCFG = ([(Cat,Double)],
                [((Cat,String),Double)],        -- terminal rules
                [((Cat,(Cat,Cat)),Double)],     -- nonterminal rules
                [Cat])

-- From Manning and Schutze, page 384
pcfg1 :: ProbCFG
pcfg1 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"astronomers"), 0.1), ((NP,"ears"), 0.18), ((NP,"saw"), 0.04), ((NP,"stars"), 0.18), ((NP,"telescopes"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.7), ((VP,(VP,PP)), 0.3),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

-- Like above but reversed probabilities on the rules for expanding VP
pcfg2 :: ProbCFG
pcfg2 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"astronomers"), 0.1), ((NP,"ears"), 0.18), ((NP,"saw"), 0.04), ((NP,"stars"), 0.18), ((NP,"telescopes"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.3), ((VP,(VP,PP)), 0.7),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allCats :: ProbCFG -> [Cat]
allCats (starting,ending,transitions,cats) = cats

--------------------------------------------------
-- Utility functions for getting information from grammars.

startProb :: ProbCFG -> Cat -> Double
startProb (starting,ending,transitions,cats) = probLookup starting

endProb :: ProbCFG -> Cat -> String -> Double
endProb (starting,ending,transitions,cats) c s = probLookup ending (c,s)

trProb :: ProbCFG -> Cat -> (Cat,Cat) -> Double
trProb (starting,ending,transitions,cats) c (c1,c2) = probLookup transitions (c,(c1,c2))

-------------------------------------------------------------
-- Simple recursive definition of inside probabilities

naiveInside :: ProbCFG -> [String] -> Cat -> Double
naiveInside pcfg [] c = undefined
naiveInside pcfg (s:[]) c = endProb pcfg c s
naiveInside pcfg list c =
    Util.sumOver (\x -> naiveRer pcfg x c) (sentStruc list (length list))

naiveRer :: ProbCFG -> ([String],[String]) -> Cat -> Double
naiveRer pcfg (s1,s2) c = Util.sumOver (\(x,y) -> (naiveInside pcfg s1 x) * (naiveInside pcfg s2 y) * (trProb pcfg c (x,y))) (Util.allPairs (allCats pcfg) (allCats pcfg))

sentStruc :: [String] -> Int -> [([String],[String])]
sentStruc list t =
    case t of
        0 -> []
        1 -> []
        _ -> sentStruc list (t-1) ++ [((take (t-1) list), (drop (t-1) list))]

allTriples :: [a] -> [b] -> [c] -> [(a,b,c)]
allTriples xs ys zs = map (\((x,y),z) -> (x,y,z)) (Util.allPairs (Util.allPairs xs ys) zs)

-------------------------------------------------------------

-- Produces a list of sentence-chunks, in the smallest-to-largest 
-- order in which their corresponding cells should be filled.
chunks :: [String] -> [[String]]
chunks sent = strN sent (length sent)

strN :: [String] -> Int -> [[String]]
strN [] t = []
strN list t =
    case t of
        0 -> []
        _ -> strN list (t-1) ++ strChunk list t

strChunk :: [String] -> Int -> [[String]]
strChunk [] t = []
strChunk list t =
    if (length list) == t then [list] else [(take t list)] ++ strChunk (drop 1 list) t
        


-- Produces a list of cells in the order in which they 
-- should be filled.
cellsToFill :: ProbCFG -> [String] -> [([String],Cat)]
cellsToFill pcfg sent = Util.allPairs (chunks sent) (allCats pcfg)

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of inside probabilities.
type InsideTable = Map.Map ([String],Cat) Double

buildTableInside :: ProbCFG -> [String] -> InsideTable
buildTableInside pcfg sent =
    Util.updateForAll (fillCellInside pcfg) Map.empty (cellsToFill pcfg sent)

fillCellInside :: ProbCFG -> InsideTable -> ([String],Cat) -> InsideTable
fillCellInside pcfg tbl (chunk,c) =
    case chunk of
    [] -> undefined
    (s:[]) ->
        let result = endProb pcfg c s in
        if result > 0 then
            Map.insert (chunk,c) result tbl
        else
            tbl
    (w1:(w2:rest)) ->
        let insideProb = \ys -> \st -> Map.findWithDefault 0 (ys,st) tbl in
        let word = sentStruc chunk (length chunk) in
        let result = Util.sumOver (\(w,c1,c2) -> (trProb pcfg c (c1,c2)) * (insideProb (fst w) c1) * (insideProb (snd w) c2)) (allTriples word (allCats pcfg) (allCats pcfg)) in
        if result > 0 then
            Map.insert (chunk,c) result tbl
        else
            tbl


-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of inside probabilities.
type ViterbiTable = (Map.Map ([String],Cat) Double, Map.Map ([String],Cat) (Cat,Cat,Int))

buildTableViterbi :: ProbCFG -> [String] -> ViterbiTable
buildTableViterbi pcfg sent =
    Util.updateForAll (fillCellViterbi pcfg) (Map.empty, Map.empty) (cellsToFill pcfg sent)

fillCellViterbi :: ProbCFG -> ViterbiTable -> ([String],Cat) -> ViterbiTable
fillCellViterbi pcfg (tblProbs,tblPointers) (chunk,c) =
    case chunk of
    [] -> undefined
    (s:[]) ->
        let result = endProb pcfg c s in
        if result > 0 then
            (Map.insert (chunk,c) result tblProbs,tblPointers)
        else
            (tblProbs,tblPointers)
    (w1:(w2:rest)) ->
        let viterbiProb = \ys -> \st -> Map.findWithDefault 0 (ys,st) tblProbs in
        let word = sentStruc chunk (length chunk) in
        let (bestProb, bestCat) = Util.maxOver (\(w,(c1,c2)) -> (trProb pcfg c (c1,c2)) * (viterbiProb (fst w) c1) * (viterbiProb (snd w) c2)) (Util.allPairs word (Util.allPairs (allCats pcfg) (allCats pcfg))) in
        if bestProb > 0 then
            (Map.insert (chunk,c) bestProb tblProbs, Map.insert (chunk,c) (fst(snd(bestCat)),snd(snd(bestCat)),length (fst(fst(bestCat)))) tblPointers)
        else
            (tblProbs, tblPointers)
-------------------------------------------------------------

-- Construct the best tree whose root is the given category and whose leaves 
-- produce the given word-sequence, based on a provided table of viterbi backpointers.
extractTree :: ([String],Cat) -> ViterbiTable -> StrucDesc
extractTree ([],c) (tblProbs,tblPointers) = undefined
extractTree (sent,c) (tblProbs,tblPointers) =
    let (a,b,d) = Map.findWithDefault undefined (sent,c) tblPointers in
        if (length sent) == 2 then
            Binary c (Leaf a (concat(take 1 sent))) (Leaf b (concat(drop 1 sent)))
        else
            if d == 1 then
                Binary c (Leaf a (concat(take 1 sent))) (extractTree ((drop 1 sent),b) (tblProbs,tblPointers))
            else
                Binary c (extractTree ((take d sent),a) (tblProbs,tblPointers)) (extractTree ((drop d sent),b) (tblProbs,tblPointers))

