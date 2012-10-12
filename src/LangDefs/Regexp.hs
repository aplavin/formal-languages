module LangDefs.Regexp(Regexp) where

import {-# SOURCE #-} LangDefs.LangDefs (LangDef (..))
import qualified Text.Regex.PCRE as PCRE
import qualified Text.Regex.Posix as Posix
import Text.Groom
import Utils
import Data.List
import Data.Char

data Regexp = Regexp {
	 name :: String
	,value :: String
	,mode :: RegExpMode
} deriving (Show)

instance LangDef Regexp where
	_name = name
	_accepted = accepted
	_representations = representations
	_fromNameLines = fromNameLines

data RegExpMode = PCRE | Posix deriving (Show, Eq)

accepted r w = w =~ value r :: Bool
	where (=~) = if mode r == PCRE then (PCRE.=~) else (Posix.=~)

representations r = zip ["Haskell definition", "Haskell regexp"] (map ($ r) [toHaskellDef, value])

fromNameLines name strs = Regexp{
		 name=name
		,mode=if "PCRE" `isSuffixOf` (map toUpper name) then PCRE else Posix
		,value=head strs
	}

toHaskellDef = groom