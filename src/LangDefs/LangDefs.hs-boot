
module LangDefs.LangDefs where

class LangDef d where
	_name :: d -> String
	_accepted :: d -> String -> Bool
	_representations :: d -> [(String, String)]
	_fromNameLines :: String -> [String] -> d