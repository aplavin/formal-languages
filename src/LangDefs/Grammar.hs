module LangDefs.Grammar where

import {-# SOURCE #-} LangDefs.LangDefs (LangDef (..))
import Data.List
import Data.List.Utils
import Data.Maybe
import Utils
import Text.Groom

type Production = (Char, String)

data Grammar = Grammar {
 	 name :: String
 	,start :: Char
 	,productions :: [Production]
} deriving (Show)

instance LangDef Grammar where
	_name = name
	_accepted = isAccepted
	_representations = representations
	_fromNameLines = fromNameLines

representations g = zip ["Haskell definition"] (map ($ g) [toHaskellDef])

toHaskellDef = groom

fromNameLines :: String -> [String] -> Grammar
fromNameLines name strs = if isCFG then Grammar{
		 name=name
		,start=fst.head $ productions
		,productions = removeEmpty productions
	} else error "Not context-free grammar"
	where
		productions :: [Production]
		productions = [(lhs, rhs) | s <- strs, let [[lhs], rhs'] = map strip $ splitOnList "->" s, let rhs = if rhs' == "e" then "" else rhs']

		isCFG :: Bool
		isCFG = all (\lhs -> length lhs == 1 && isN (head lhs)) $ map (strip.head) $ map (splitOnList "->") strs

isN :: Char -> Bool
isN c = 'A' <= c && c <= 'Z'

isT :: Char -> Bool
isT = not . isN

terminals :: Grammar -> [Char]
terminals g = foldl union [] (map (filter isT.snd) $ productions g)

nonterminals :: Grammar -> [Char]
nonterminals = nub . map fst . productions

symbols :: Grammar -> [Char]
symbols g = terminals g `union` nonterminals g

removeEmpty :: [Production] -> [Production]
removeEmpty prods = [(lhs, rhs') | (lhs, rhs) <- noEmptyProds, rhs' <- allReplaces rhs]
	where
		emptyProds = filter (null.snd) prods
		noEmptyProds = filter (not.null.snd) prods

		allReplaces prod
			| null eProds = [prod]
			| otherwise = prod : concat [allReplaces prod' | eProd <- eProds, let prod' = replaceOnce [eProd] [] prod]
			where eProds = filter (`elem` prod) $ map fst emptyProds

isAccepted :: Grammar -> String -> Bool
isAccepted g w = isAccepted' g w []

isAccepted' :: Grammar -> String -> String -> Bool
isAccepted' g w stack
	| null w = [start g] `elem` stacks'
	| otherwise = or [isAccepted' g (tail w) st | st <- stacks']
	where
		stack' = if null w then stack else head w : stack
		stacks' = replaceProds stack'

		prodsOnTop st = filter (\prod -> reverse (snd prod) `isPrefixOf` st) (productions g)
		replaceProds st
			| null prods = [st]
			| otherwise = st : concat [replaceProds (fst prod : drop (length $ snd prod) st) | prod <- prods]
			where prods = prodsOnTop st

