module ProbFSAv2 where

import qualified Util
import qualified Data.Map as Map

type State = Int

type ProbFSA = ([(State,Double)],                   -- start distribution
                [(State,Double)],                   -- ending probabilities
                [((State,State),Double)],           -- transition probabilities
                [(((State,State),String),Double)],  -- emissions
                [State])                            -- all states

-- This is a probabilistic version of a grammar from Assignment #4.
pfsa1 :: ProbFSA
pfsa1 = (   -- start distribution
            [(1,1.0)] ,
            -- end probabilities
            [(5,0.5)] ,
            -- transition probabilities
            [((1,2), 0.3), ((1,3), 0.7),
             ((2,3), 1.0),
             ((3,4), 1.0),
             ((4,4), 0.4),
             ((4,5), 0.6),
             ((5,1), 0.5)] ,
            -- emission probabilities
            [(((1,2),"these"), 0.5), (((1,2),"some"), 0.5),
             (((1,3),"they"), 0.4), (((1,3),"these"), 0.6),
             (((2,3),"dogs"), 0.3), (((2,3),"buffalo"), 0.7),
             (((3,4),"buffalo"), 0.6), (((3,4),"damaged"), 0.4),
             (((4,4),"damaged"), 0.7), (((4,4),"nice"), 0.3),
             (((4,5),"unicorns"), 0.8), (((4,5),"stuff"), 0.2),
             (((5,1),"and"),0.1)],
            [1,2,3,4,5]
        )

-- This is the grammar we worked with in class this week.
pfsa2 :: ProbFSA
pfsa2 = (   -- start distribution
            [(100,1.0)] ,
            -- end probabilities
            [(400,0.5)] ,
            -- transition probabilities
            [((100,200), 0.4), ((100,300), 0.6),
             ((200,400), 1.0),
             ((300,200), 0.3), ((300,400), 0.7),
             ((400,300), 0.5)] ,
            -- emission probabilities
            [(((100,200),"c"), 1.0), 
             (((100,300),"a"), 0.7), (((100,300),"b"), 0.3), 
             (((200,400),"a"), 0.2), (((200,400),"d"), 0.8), 
             (((300,200),"c"), 0.6), (((300,200),"d"), 0.4), 
             (((300,400),"b"), 0.5), (((300,400),"c"), 0.5), 
             (((400,300),"b"), 0.3), (((400,300),"d"), 0.7)] ,
            [100,200,300,400]
        )

-- This is the grammar introduced at the beginning of Assignment #9.
pfsa3 :: ProbFSA
pfsa3 = (   -- start distribution
            [(10,1.0)] ,
            -- end probabilities
            [(40,0.5)] ,
            -- transition probabilities
            [((10,20), 0.4), ((10,30), 0.6),
             ((20,40), 1.0),
             ((30,40), 1.0),
             ((40,10), 0.5)] ,
            -- emission probabilities
            [(((10,20),"a"), 0.2), (((10,20),"b"), 0.8), 
             (((10,30),"a"), 0.7), (((10,30),"c"), 0.3), 
             (((20,40),"c"), 0.6), (((20,40),"d"), 0.4), 
             (((30,40),"b"), 0.1), (((30,40),"d"), 0.9), 
             (((40,10),"e"), 1.0)] ,
            [10,20,30,40]
        )

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allStates :: ProbFSA -> [State]
allStates (starting,ending,transitions,emissions,states) = states

--------------------------------------------------
-- Utility functions for getting information from grammars.

startProb :: ProbFSA -> State -> Double
startProb (starting,ending,transitions,emissions,states) = probLookup starting

endProb :: ProbFSA -> State -> Double
endProb (starting,ending,transitions,emissions,states) = probLookup ending

trProb :: ProbFSA -> State -> State -> Double
trProb (starting,ending,transitions,emissions,states) st1 st2 = probLookup transitions (st1,st2)

emProb :: ProbFSA -> (State,State) -> String -> Double
emProb (starting,ending,transitions,emissions,states) (st1,st2) str = probLookup emissions ((st1,st2),str)

--------------------------------------------------
-- Simple recursive definition of backward probabilities from last week

naiveBackward :: ProbFSA -> [String] -> State -> Double
naiveBackward pfsa []     st = endProb pfsa st
naiveBackward pfsa (w:ws) st =
    Util.sumOver (\next -> trProb pfsa st next * emProb pfsa (st,next) w * naiveBackward pfsa ws next) (allStates pfsa)

--------------------------------------------------

suffixes :: [String] -> [[String]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : (suffixes xs)

-- Produces a list of sentence-chunks, in the smallest-to-largest 
-- order in which their corresponding cells should be filled.
chunks :: [String] -> [[String]]
chunks sent = reverse (suffixes sent)

-- Produces a list of cells in the order in which they 
-- should be filled.
cellsToFill :: ProbFSA -> [String] -> [([String],State)]
cellsToFill pfsa sent = Util.allPairs (chunks sent) (allStates pfsa)

--------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of backward probabilities.
type BackwardTable = Map.Map ([String],State) Double

buildTableBackward :: ProbFSA -> [String] -> BackwardTable
buildTableBackward pfsa sent =
    Util.updateForAll (fillCellBackward pfsa) Map.empty (cellsToFill pfsa sent)

fillCellBackward :: ProbFSA -> BackwardTable -> ([String],State) -> BackwardTable
fillCellBackward pfsa tbl (chunk,s) =
    case chunk of
    [] ->
        let result = endProb pfsa s in
        if result > 0 then
            Map.insert (chunk,s) result tbl
        else
            tbl
    (w:ws) ->
        let backwardProb = \ys -> \st -> Map.findWithDefault 0 (ys,st) tbl in
        let result = Util.sumOver (\next -> trProb pfsa s next * emProb pfsa (s,next) w * backwardProb ws next) (allStates pfsa) in
        if result > 0 then
            Map.insert (chunk,s) result tbl
        else
            tbl

--------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of viterbi probabilities.
type ViterbiTable = (Map.Map ([String],State) Double, Map.Map ([String],State) State)

buildTableViterbi :: ProbFSA -> [String] -> ViterbiTable
buildTableViterbi pfsa sent =
    Util.updateForAll (fillCellViterbi pfsa) (Map.empty, Map.empty) (cellsToFill pfsa sent)

fillCellViterbi :: ProbFSA -> ViterbiTable -> ([String],State) -> ViterbiTable
fillCellViterbi pfsa (tblProbs,tblPointers) (chunk,s) =
    case chunk of
    [] ->
        let result = endProb pfsa s in
        if result > 0 then
            (Map.insert (chunk,s) result tblProbs, tblPointers)
        else
            (tblProbs, tblPointers)
    (w:ws) ->
        let viterbiProb = \ys -> \st -> Map.findWithDefault 0 (ys,st) tblProbs in
        let (bestProb, bestState) = Util.maxOver (\next -> trProb pfsa s next * emProb pfsa (s,next) w * viterbiProb ws next) (allStates pfsa) in
        if bestProb > 0 then
            (Map.insert (chunk,s) bestProb tblProbs, Map.insert (chunk,s) bestState tblPointers)
        else
            (tblProbs, tblPointers)

