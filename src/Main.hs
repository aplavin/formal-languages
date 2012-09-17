import System.IO
import Text.Regex.Posix
import Text.Printf
import Data.List
import Data.Maybe
import Data.List.Utils
import Data.Tuple.Curry
import Control.Monad
import LangDefs
import FileParser
import Grammar
import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DFA
import qualified NFA

genNth _ 0 _ = ""
genNth alphabet len n = genNth alphabet (len-1) q ++ [alphabet !! r]
	where (q, r) = n `quotRem` length alphabet
	
gen alphabet = [genNth alphabet len n | len <- [0..], n <- [0..length alphabet ^ len - 1]]

input = "input.txt"
output = "result.txt"
width = 100

createOutput = writeFile output ""

outputLine line = appendFile output (line ++ "\n")


langDefRepr ldef = justifyCenter width '-' (" " ++ name ldef ++ " ") ++ "\n" ++ intercalate "\n" [printf "%s\n%s\n%s" (fmtName name) (replicate width '-') text | (name, text) <- reprs]
	where
		reprs = representations ldef
		fmtName name = justifyRight width ' ' ("-- " ++ name ++ " --")

wordsAcceptance settings langdefs = 
	map (\var -> variantStr var ++ "\n" ++ intercalate ", " (variantWords var) ++ "\n") variants
	where
		generated = take (numWordsGen settings) (gen $ alphabet settings)
		replaceEps = map (\w -> if w == "" then "\\eps" else w)

		variants = 
			[[if i == ci then Just True else Nothing | i <- [1..length langdefs]] | ci <- [1..length langdefs]]
			++ [[if i == ci then Just False else Nothing | i <- [1..length langdefs]] | ci <- [1..length langdefs]]


		variantStr var = concat [if isNothing v then "" else if fromJust v then " +" ++ name else " -" ++ name | (v, name) <- zip var (map name langdefs)]
		variantCond var w = and [if v then accepted ldef w else not (accepted ldef w) | (Just v, ldef) <- zip var langdefs]

		variantWords var = replaceEps $ filter (variantCond var) generated


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
	print $ wordsAcceptance settings langdefs
	outputLine $ intercalate "\n" $ wordsAcceptance settings langdefs