module Grammar where

import Data.List
import Data.Maybe

--data NonTerm = S | Q0 | Q1 | Q2 | Q3 deriving (Show, Eq)
--data Term = EPS | T0 | T1 deriving (Show, Eq)
data Item = S | Q0 | Q1 | Q2 | Q3 | EPS | T0 | T1 deriving (Show, Eq)

terms = [
	 (EPS, "")
	,(T0, "0")
	,(T1, "1")
	]

tts t = s where (_, s) = fromJust $ find (\(t', _) -> t' == t) terms

rules = [
	 (S, [Q2])
	,(Q0, [EPS])
	,(Q0, [Q0, T0])
	,(Q0, [Q1, T1])
	,(Q0, [Q3, T0])
	,(Q1, [Q0, T1])
	,(Q1, [Q2, T0])
	,(Q2, [Q1, T0])
	,(Q2, [Q2, T1])
	,(Q1, [Q3, T1])
	,(Q3, [Q0, T0])
	,(Q3, [Q3, T0])
	]

maxsteps = 10

check terms rules n nonterm word
	| n <= 0 = False
	| maybe False (\(t, _) -> (nonterm, [t]) `elem` rules) (find (\(_, s) -> s == word) terms) = True
	| otherwise = or [check terms rules (n-1) nonterm' w | (nt, repl) <- filter (\(f, _) -> f == nonterm) rules, length repl == 1 || (tts $ last repl) `isSuffixOf` word, let w = if length repl == 1 then word else init word, let nonterm' = head repl]