module LangDefs.Grammar where

import {-# SOURCE #-} LangDefs.LangDefs (LangDef (..))
import Data.List
import Data.List.Utils
import Data.Maybe
import Utils
import Text.Groom

data Grammar = Grammar {
 	 name :: String
 	,start1 :: [Char]
 	,productions :: [([Char], [Char])]
}

instance LangDef Grammar where
	_name = name
	_accepted = accepted
	_representations = representations
	_fromNameLines = fromNameLines

accepted g w
	| isCFG g = isAccepted (removeEmpty (head $ start1 g, [(from, to) | ([from], to) <- productions g])) w
	| otherwise = error "Not context-free"

isCFG g = all (\(from, to) -> length from == 1) (productions g)

representations g = zip ["Haskell definition"] (map ($ g) [toHaskellDef])

toHaskellDef g@(Grammar{}) = groom (removeEmpty (head $ start1 g, [(from, to) | ([from], to) <- productions g]))

fromNameLines name strs = Grammar{
		 name=name
		,start1=fst.head $ productions
		,productions=productions
	}
	where 
		productions = [(lhs, rhs) | s <- strs, let [lhs, rhs'] = map strip $ splitOnList "->" s, let rhs = if rhs' == "e" then "" else rhs']

class (Show s, Eq s) => Symbol s where
	isT :: s -> Bool
	isN :: s -> Bool
	isT = not . isN
	isN = not . isT

instance Symbol Char where
	isN c = 'A' <= c && c <= 'Z'

type CFG s = (s, [(s, [s])])

start :: CFG s -> s
start = fst

prods :: CFG s -> [(s, [s])]
prods = snd

terminals :: Symbol s => CFG s -> [s]
terminals g = foldl union [] (map (filter isT.snd) $ prods g)

nonterminals :: Symbol s => CFG s -> [s]
nonterminals = nub . map fst . prods

symbols :: Symbol s => CFG s -> [s]
symbols g = terminals g `union` nonterminals g

removeEmpty :: Symbol s => CFG s -> CFG s
removeEmpty g = (start g, [(lhs, rhs') | (lhs, rhs) <- noEmptyProds, rhs' <- allReplaces rhs])
	where
		emptyProds = filter (null.snd) (prods g)
		noEmptyProds = filter (not.null.snd) (prods g)

		allReplaces prod
			| null eProds = [prod]
			| otherwise = prod : concat [allReplaces prod' | eProd <- eProds, let prod' = replaceOnce [eProd] [] prod]
			where eProds = filter (`elem` prod) $ map fst emptyProds

isAccepted :: Symbol s => CFG s -> [s] -> Bool
isAccepted g w = isAccepted' g w []

isAccepted' :: Symbol s => CFG s -> [s] -> [s] -> Bool
isAccepted' g w stack
	| null w = [start g] `elem` stacks'
	| otherwise = or [isAccepted' g (tail w) st | st <- stacks']
	where
		stack' = if null w then stack else head w : stack
		stacks' = replaceProds stack'

		prodsOnTop st = filter (\prod -> reverse (snd prod) `isPrefixOf` st) (prods g)
		replaceProds st
			| null prods = [st]
			| otherwise = st : concat [replaceProds (fst prod : drop (length $ snd prod) st) | prod <- prods]
			where prods = prodsOnTop st