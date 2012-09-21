module FileParser(parseFile, Settings(..)) where

import Data.List
import Data.Ord
import Text.Regex.Posix
import LangDefs.LangDefs
import Utils

-- | Settings for generating and checking words
data Settings = Settings {
	numWordsGen :: Int,
	alphabet :: String
} deriving (Show)

-- | Parse specified file
parseFile :: FilePath -> IO (Settings, [LangDef])
parseFile file = do
	content <- readFile file
	let fileLines = map strip $ lines content
	let blocks = splitBlocks fileLines

	let settings = parseSettings $ head blocks
	let langdefs = map fromStrings $ tail blocks

	return (settings, langdefs)

	where
		emptyOrComment line = null line || "#" `isPrefixOf` line
		splitBlocks = splitWhen emptyOrComment

parseSettings :: [String] -> Settings
parseSettings strings = Settings{numWordsGen = numWords, alphabet = alphabet}
	where
		numWords = read (head strings =~ " [0-9]+ ")
		alphabet = map head $ splitOneOf ", " $ last.head $ (head strings =~ "{(.*)}" :: [[String]])