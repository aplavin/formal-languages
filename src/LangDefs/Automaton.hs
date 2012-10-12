module LangDefs.Automaton (Automaton (..)) where

import {-# SOURCE #-} LangDefs.LangDefs (LangDef (..))
import qualified LangDefs.DFA as DFA
import qualified LangDefs.NFA as NFA
import Utils
import Text.Groom
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf

data Automaton = Automaton {
	 name :: String
	,a_alphabet :: [Char]
	,states :: [String]
	,startStates :: [String]
	,acceptStates :: [String]
	,delta :: [(String, Maybe Char, [String])]
}

instance LangDef Automaton where
	_name = name
	_accepted = accepted
	_representations = representations
	_fromNameLines = fromNameLines

accepted a w
	| isDFA a = DFA.accept w (getDFA a)
	| otherwise = NFA.accept w (getNFA a)

representations a = zip ["Haskell definition", "Table", "LaTeX table"] (map ($ a) [toHaskellDef, toTable, toLatexTable])

fromNameLines name strs = Automaton{
		 name = name
		,a_alphabet = filter (/='e') alphabet
		,states = states
		,startStates = startStates
		,acceptStates = acceptStates
		,delta = delta
	}
	where
		alphabet = map head . lineBlocks . head $ strs

		stateLines = map lineBlocks $ tail strs

		lineBlocks = splitOn '\t'
		stripState = stripOneOf " ><F"

		states = map (stripState . head) stateLines
		startStates = map stripState . filter (">" `isInfixOf`) . map head $ stateLines
		acceptStates = map stripState . filter ("F" `isInfixOf`) . map head $ stateLines

		delta = [(stripState st, ch', states') |
			 (st : stLine) <- stateLines
			,(ch, states') <- zip alphabet (map (splitOn ',') stLine)
			,let ch' = if ch /= 'e' then Just ch else Nothing
			,states' /= ["-"]]

isDFA a = all (\(_, c, states) -> isJust c && length states <= 1) (delta a) && (length (startStates a) == 1)

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

toHaskellDef a@(Automaton{})
	| isDFA a = groom (getDFA a)
	| otherwise = groom (getNFA a)

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
