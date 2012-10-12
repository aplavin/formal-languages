module LangDefs.LangDefs (LangDef(..), LangDefDict(..), fromLines) where

import Text.Regex.Posix
import Utils
import Data.List
import qualified LangDefs.Grammar as Grammar
import qualified LangDefs.Automaton as Automaton
import qualified LangDefs.Regexp as Regexp

class LangDef d where
	_name :: d -> String
	_accepted :: d -> String -> Bool
	_representations :: d -> [(String, String)]
	_fromNameLines :: String -> [String] -> d

data LangDefDict = LangDefDict {
	 name :: String
	,accepted :: String -> Bool
	,representations :: [(String, String)]
}

buildDict :: LangDef ld => ld -> LangDefDict
buildDict d = LangDefDict { name = _name d, accepted = _accepted d, representations = _representations d }

fromLines :: [String] -> LangDefDict
fromLines (sName: strs) = case head name of
			'A' -> buildDict (_fromNameLines name strs :: Automaton.Automaton)
			'R' -> buildDict (_fromNameLines name strs :: Regexp.Regexp)
			'G' -> buildDict (_fromNameLines name strs :: Grammar.Grammar)
	where
		name = sName =~ "[A-Za-z0-9_-]+"

