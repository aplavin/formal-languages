module LangDefs.Regexp where

import qualified Text.Regex.PCRE as PCRE
import qualified Text.Regex.Posix as Posix
import Text.Groom
import Utils
import Data.List
import Data.Char

data Regexp = RegExp {
	 name :: String
	,value :: String
	,mode :: RegExpMode
} deriving (Show)

data RegExpMode = PCRE | Posix deriving (Show, Eq)

accepted r w = w =~ value r :: Bool
	where (=~) = if mode r == PCRE then (PCRE.=~) else (Posix.=~)

representations r = zip ["Haskell definition", "Haskell regexp"] (map ($ r) [toHaskellDef, value])

fromNameLines name strs = RegExp{
		 name=name
		,mode=if "PCRE" `isSuffixOf` (map toUpper name) then PCRE else Posix
		,value=head strs
	}

toHaskellDef = groom