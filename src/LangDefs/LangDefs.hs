module LangDefs.LangDefs where

import qualified Text.Regex.PCRE as PCRE
import qualified Text.Regex.Posix as Posix
import Text.Printf
import Data.Maybe
import Data.List
import Text.Groom
import Data.Char
import Utils
import LangDefs.Grammar
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified LangDefs.DFA as DFA
import qualified LangDefs.NFA as NFA

data LangDef = Automaton {
	 name :: String
	,a_alphabet :: [Char]
	,states :: [String]
	,startStates :: [String]
	,acceptStates :: [String]
	,delta :: [(String, Maybe Char, [String])]
	,fa :: Maybe (Either (DFA.DFA String Char) (NFA.NFA String Char))
}
 | RegExp {
	 name :: String
	,value :: String
	,mode :: RegExpMode
}
 | Grammar {
 	 name :: String
 	,start :: [Char]
 	,productions :: [([Char], [Char])]
}
 deriving (Show, Eq)

data RegExpMode = PCRE | Posix deriving (Show, Eq)

accepted a@(Automaton{}) w
	| isDFA a = DFA.accept w (getDFA a)
	| otherwise = NFA.accept w (getNFA a)

accepted r@(RegExp{mode = PCRE}) w = w PCRE.=~ value r :: Bool
accepted r@(RegExp{mode = Posix}) w = w Posix.=~ value r :: Bool

accepted g@(Grammar{}) w
	| isCFG g = isAccepted (removeEmpty (head $ start g, [(from, to) | ([from], to) <- productions g])) w
	| otherwise = error "Not context-free"

representations a@(Automaton{}) = zip ["Haskell definition", "Table", "LaTeX table"] (map ($ a) [toHaskellDef, toTable, toLatexTable])
representations r@(RegExp{}) = zip ["Haskell definition", "Haskell regexp"] (map ($ r) [toHaskellDef, value])
representations g@(Grammar{}) = zip ["Haskell definition"] (map ($ g) [toHaskellDef])

fromStrings strs
	| "A" `isPrefixOf` name = Automaton{
		 name = name
		,a_alphabet = alphabet
		,states = states
		,startStates = startStates
		,acceptStates = acceptStates
		,delta = delta
		,fa = Nothing --if isDFA then getDFA a else getNFA a
	}
	| "R" `isPrefixOf` name = RegExp{
		 name=name
		,mode=if "PCRE" `isSuffixOf` (map toUpper name) then PCRE else Posix
		,value=head.tail $ strs
	}
	| "G" `isPrefixOf` name = Grammar{
		 name=name
		,start=fst.head $ productions
		,productions=productions
	}
	where
		name = head strs PCRE.=~ "\\w+"

		alphabet = filter (/= 'e') $ map head $ splitOneOf " \t" . head . tail $ strs
		stateLines = map (\line -> stripOneOf " ><F" (head (lineBlocks line)) : tail (lineBlocks line)) $ drop 2 strs where lineBlocks = splitOn '\t'
		states = map head stateLines
		startStates = map (stripOneOf " ><F") . filter (">" `isInfixOf`) . map (head . splitOn '\t') $ drop 2 strs
		acceptStates = map (stripOneOf " ><F") . filter ("F" `isInfixOf`) . map (head . splitOn '\t') $ drop 2 strs
		delta = [(st, ch', states') |
			 (st : stLine) <- stateLines
			,(ch, states') <- zip alphabet (map (splitOn ',') stLine)
			,let ch' = if ch /= 'e' then Just ch else Nothing
			,states' /= ["-"]]

		productions = [(lhs, rhs) | s <- tail strs, let [lhs, rhs'] = map strip $ splitOnList "->" s, let rhs = if rhs' == "e" then "" else rhs']

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
	
-- | Get Haskell code definition for this automaton
toHaskellDef a@(Automaton{})
	| isDFA a = groom (getDFA a)
	| otherwise = groom (getNFA a)
toHaskellDef r@(RegExp{}) = groom r
toHaskellDef g@(Grammar{}) = groom (removeEmpty (head $ start g, [(from, to) | ([from], to) <- productions g])) --g
	
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

isCFG g = all (\(from, to) -> length from == 1) (productions g)