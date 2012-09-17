module Automaton where

import Data.Maybe
import Data.List
import Text.Groom
import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DFA
import qualified NFA

data Automaton = Automaton {
	 name :: String
	,alphabet :: [Char]
	,states :: [String]
	,startStates :: [String]
	,acceptStates :: [String]
	,delta :: [(String, Maybe Char, [String])]
} deriving Show

isDFA a = (all id $ map (\(_, _, states) -> length states <= 1) $ delta a) && (length (startStates a) == 1)

getDFA a = if not (isDFA a) then error "Automaton is not DFA" else DFA.DFA
    { DFA.states = Set.fromList (states a)
    , DFA.sigma = Set.fromList (alphabet a)
    , DFA.delta = Map.fromList [((from, char), to) | (from, Just char, [to]) <- delta a]
    , DFA.startState = head (startStates a)
    , DFA.acceptStates = Set.fromList (acceptStates a) }
	
getNFA a = NFA.NFA
    { NFA.states = Set.fromList (states a)
    , NFA.sigma = Set.fromList (alphabet a)
    , NFA.delta = Map.fromList  [((from, char), Set.fromList to) | (from, char, to) <- delta a]
    , NFA.startState = head (startStates a)
    , NFA.acceptStates = Set.fromList (acceptStates a) }
	
isAccepted a w
	| isDFA a = DFA.accept w (getDFA a)
	| otherwise = NFA.accept w (getNFA a)
	
representations a = [(name, func a) | (name, func) <- zip ["Haskell definition", "Table", "LaTeX table"] [toHaskellDef, toTable, toLatexTable]]
	
-- | Get Haskell code definition for this automaton
toHaskellDef a
	| isDFA a = groom (getDFA a)
	| otherwise = groom (getNFA a)
	
-- | Get table definition for this automaton, format similar to input
toTable a = alphabetS ++ "\n" ++ intercalate "\n" statesS
	where
		alphabetS = '\t' : intersperse '\t' (alphabet a)
		statesS = [(if isStart then ">" else "") ++ st ++ (if isAccept then "F" else "") ++ "\t" ++ intercalate "\t" transitions|
			 st <- sort (states a)
			,let isStart = st `elem` startStates a
			,let isAccept = st `elem` acceptStates a
			,let transitions = [intercalate "," to | char <- alphabet a, let findRes = find (\(st', ch, _) -> st' == st && fromMaybe 'e' ch == char) (delta a),  let to = maybe ["-"] thrd findRes]
			]
		
-- | Get LaTeX representation for this automaton table		
toLatexTable a = unlines [
					 "\\begin{tabular}{" ++ colsDef ++ "}"
					,"\\hline"
					,alphabetS ++ "\\\\"
					,"\\hline"
					,intercalate "\\\\\n" statesS ++ "\\\\"
					,"\\hline"
					,"\\end{tabular}"]
	where
		colsDef = "|l|" ++ intersperse '|' (replicate (length $ alphabet a) 'c') ++ "|"
		alphabetS = '&' : (intercalate "&" . map (surround " $" "$ ")) (alphabet a)
		statesS = [(intercalate "&" . map (surround " $" "$ ")) $ ((if isStart then "\\ra " else "") ++ st ++ (if isAccept then " F" else "")) : transitions |
			 st <- sort (states a)
			,let isStart = st `elem` startStates a
			,let isAccept = st `elem` acceptStates a
			,let transitions = [intercalate "," to | (from, char, to) <- delta a, from == st]
			]