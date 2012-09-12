module Automaton where

data Automaton = Automaton {
	 name :: String
	,alphabet :: [Char]
	,states :: [String]
	,startStates :: [String]
	,acceptStates :: [String]
	,delta :: [(String, Char, [String])]
} deriving Show