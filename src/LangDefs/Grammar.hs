module Grammar(Symbol, CFG, removeEmpty, isAccepted) where

import Data.List
import Data.List.Utils
import Data.Maybe
import Debug.Observe
import Debug.Trace
import Control.DeepSeq
import Utils

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