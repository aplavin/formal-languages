import System.IO
import Text.Regex.Posix
import Text.Printf
import Data.List
import Data.Maybe
import Data.List.Utils
import Data.Tuple.Curry
import Control.Monad
import LangDefs.LangDefs
import FileParser
import LangDefs.Grammar
import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified LangDefs.DFA as DFA
import qualified LangDefs.NFA as NFA

genNth _ 0 _ = ""
genNth alphabet len n = genNth alphabet (len-1) q ++ [alphabet !! r]
	where (q, r) = n `quotRem` length alphabet
	
gen alphabet = [genNth alphabet len n | len <- [0..], n <- [0..length alphabet ^ len - 1]]

input = "input.txt"
output = "result.txt"
width = 80

createOutput = writeFile output ""

outputLine line = appendFile output (line ++ "\n")

-- | Representation of LangDef in pretty format, with headers and so on
langDefRepr ldef = justifyCenter width '-' (" " ++ name ldef ++ " ") ++ "\n" ++ intercalate "\n" [printf "%s\n%s\n%s" (fmtName name) (replicate width '-') text | (name, text) <- reprs]
	where
		reprs = representations ldef
		fmtName name = justifyRight width ' ' ("-- " ++ name ++ " --")

wordsAcceptance settings langdefs = 
	map formatVariant variants
	where
		generated = take (numWordsGen settings) (gen $ alphabet settings)
		replaceEps = map (\w -> if w == "" then "\\eps" else w)

		allNothing = replicate (length langdefs) Nothing

		-- tail because we don't want to have a list of Nothing's
		variants = sortBy cmpVariants $ reverse $ tail $ cartesianProduct (length langdefs) [Nothing, Just True, Just False]

		cmpVariants v1 v2
			| count Nothing v1 /= count Nothing v2 = count Nothing v2 `compare` count Nothing v1 -- reverse order
			| allEqual v1 /= allEqual v2 = allEqual v2 `compare` allEqual v1 -- reverse order
			| count (Just True) v1 /= count (Just True) v2 = count (Just True) v2 `compare` count (Just True) v1 -- reverse order
			| otherwise = EQ -- don't change order

		variantStr var
			| allEqual var = (if fromJust (head var) then "+" else "-") ++ intercalate ", " (map name langdefs)
			| otherwise =  tail $ concat [if isNothing v then "" else if fromJust v then " +" ++ name else " -" ++ name | (v, name) <- zip var (map name langdefs)]
		variantCond var w = and [if v then accepted ldef w else not (accepted ldef w) | (Just v, ldef) <- zip var langdefs]
		variantWords var = replaceEps $ filter (variantCond var) generated

		formatVariant var = printf "-- [ %s ] %d / %d --\n%s\n" str (length varWords) (numWordsGen settings) wordsStr
			where
				str = variantStr var
				varWords = variantWords var
				wordsStr = if null varWords then "None" else intercalate ", " varWords


main = do
	(settings, langdefs) <- parseFile input
	createOutput

	outputLine $ justifyCenter width '=' " Representations "
	outputLine $ replicate width '='
	outputLine ""
	outputLine $ unlines $ map langDefRepr langdefs

	outputLine $ justifyCenter width '=' " Words acceptance results "
	outputLine $ justifyCenter width '=' (printf " Alphabet: {%s} " (intersperse ',' (alphabet settings)))
	outputLine ""
	outputLine $ unlines $ wordsAcceptance settings langdefs