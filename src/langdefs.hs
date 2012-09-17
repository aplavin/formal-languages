module LangDefs where

import Text.Regex.Posix
import Text.Printf
import Data.Maybe
import Data.List
import Text.Groom
import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DFA
import qualified NFA

data LangDef = Automaton {
	 name :: String
	,a_alphabet :: [Char]
	,states :: [String]
	,startStates :: [String]
	,acceptStates :: [String]
	,delta :: [(String, Maybe Char, [String])]
}
 | RegExp {
	 name :: String
	,value :: String
}
 deriving (Show, Eq)

accepted a@(Automaton{}) w
	| isDFA a = DFA.accept w (getDFA a)
	| otherwise = NFA.accept w (getNFA a)
accepted r@(RegExp{}) w = w =~ value r :: Bool

representations a@(Automaton{}) = zip ["Haskell definition", "Table", "LaTeX table"] (map ($ a) [toHaskellDef, toTable, toLatexTable])
representations r@(RegExp{}) = [("Haskell regexp", value r)]

isDFA a = (all id $ map (\(_, _, states) -> length states <= 1) $ delta a) && (length (startStates a) == 1)

getDFA a = if not (isDFA a) then error "Automaton is not DFA" else DFA.DFA
    { DFA.states = Set.fromList (states a)
    , DFA.sigma = Set.fromList (a_alphabet a)
    , DFA.delta = Map.fromList [((from, char), to) | (from, Just char, [to]) <- delta a]
    , DFA.startState = head (startStates a)
    , DFA.acceptStates = Set.fromList (acceptStates a) }
	
getNFA a = NFA.NFA
    { NFA.states = Set.fromList (states a)
    , NFA.sigma = Set.fromList (a_alphabet a)
    , NFA.delta = Map.fromList  [((from, char), Set.fromList to) | (from, char, to) <- delta a]
    , NFA.startState = head (startStates a)
    , NFA.acceptStates = Set.fromList (acceptStates a) }
	
-- | Get Haskell code definition for this automaton
toHaskellDef a
	| isDFA a = groom (getDFA a)
	| otherwise = groom (getNFA a)
	
-- | Get table definition for this automaton, format similar to input
toTable a = "\t" ++ intersperse '\t' alphabet' ++ "\n" ++ intercalate "\n" statesS
	where
		-- are there any Eps transitions
		anyEpsTrans = any (\(_, c, _) -> isNothing c) (delta a)
		-- list of alphabet symbols, prepended with 'e' if Eps transitions exist
		alphabet' = if anyEpsTrans then 'e' : a_alphabet a else a_alphabet a
		-- list of lines which consist of state and transitions from it (order as in alphabet)
		statesS = [printf (fmtStr st) st ++ "\t" ++ intercalate "\t" transitions |
			-- just go through sorted list of states
			 st <- sort (states a)
			-- format string to get '>' before or 'F' after state
			,let fmtStr st = (if isStart st then ">" else "") ++ "%s" ++ (if isAccept st then "F" else "")
			,let transitions = [intercalate "," to | 
				 char <- [Nothing | anyEpsTrans] ++ map return (a_alphabet a)
				,let findRes = find (\(st', ch, _) -> st' == st && ch == char) (delta a)
				,let to = maybe ["-"] thrd findRes]
			]
		isStart st = st `elem` startStates a
		isAccept st = st `elem` acceptStates a
		
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
		colsDef = "|l|" ++ intersperse '|' (replicate (length $ a_alphabet a) 'c') ++ "|"
		alphabetS = '&' : (intercalate "&" . map (surround " $" "$ ")) (a_alphabet a)
		statesS = [(intercalate "&" . map (surround " $" "$ ")) $ ((if isStart st then "\\ra " else "") ++ st ++ (if isAccept st then " F" else "")) : transitions |
			 st <- sort (states a)
			,let transitions = [intercalate "," to | (from, char, to) <- delta a, from == st]
			]
		isStart st = st `elem` startStates a
		isAccept st = st `elem` acceptStates a