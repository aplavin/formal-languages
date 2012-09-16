import System.IO
import Text.Regex.Posix
import Text.Printf
import Data.List
import Data.List.Utils
import Data.Tuple.Curry
import Automaton
import FileParser
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DFA
import qualified NFA

genNth _ 0 _ = ""
genNth alphabet len n = genNth alphabet (len-1) q ++ [alphabet !! r]
	where (q, r) = n `quotRem` length alphabet
	
gen alphabet = [genNth alphabet len n | len <- [0..], n <- [0..length alphabet ^ len - 1]]

input = "automata.txt"
output = "result.txt"

createOutput = writeFile output ""

outputLine line = appendFile output (line ++ "\n")
outputLineS val = appendFile output (show val ++ "\n")

automatonRepr settings a = 
	unlines $ intersperse (replicate 80 '-') $ [
		 "== Automaton " ++ name a ++ " =="
		,"- Representations -"]
		++ map reprToString (representations a)
		++ map (uncurryN formatWords) [
			 ("Accepted", "A", accepted)
			,("Accepted", "REGEX", acceptedRegex)
			,("Rejected", "A", rejected)
			,("Rejected", "REGEX", rejectedRegex)
			,("Diff", "A \\ REGEX", acceptedNotRegex)
			,("Diff", "REGEX \\ A", rejectedAndRegex)]
	where
		reprToString (name, text) = intercalate "\n" [name ++ ":", replicate (length name + 1) '-', text]
		
		generated = take (numWordsGen settings) (gen $ alphabet a)
		replaceEps = map (\w -> if w == "" then "\\eps" else w)
		
		accepted = replaceEps $ filter (isAccepted a) generated
		rejected = replaceEps $ filter (not.isAccepted a) generated
		acceptedRegex = replaceEps $ filter (\w -> (w =~ (regexp settings) :: Bool)) generated
		rejectedRegex = replaceEps $ filter (\w -> not (w =~ (regexp settings) :: Bool)) generated
		
		acceptedNotRegex = replaceEps $ filter (\w -> isAccepted a w && not (w =~ (regexp settings) :: Bool)) generated
		rejectedAndRegex = replaceEps $ filter (\w -> not (isAccepted a w) && (w =~ (regexp settings) :: Bool)) generated
		
		formatWords acc source words = printf "%s words [%s] (%d of first %d):\n%s" acc source (length words) (numWordsGen settings) (if (not.null) words then intercalate ", " words else "None")

main = do
	(settings, automata) <- parseFile input
	createOutput
	outputLine $ unlines $ intersperse (replicate 80 '=') $ map (automatonRepr settings) automata