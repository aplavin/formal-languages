module FileParser(parseFile, Settings(..)) where

import Data.List
import Data.Ord
import Text.Parsec hiding (space, spaces)
import Text.Parsec.String
import qualified Data.List.Split as Split
import LangDefs

-- | Settings for generating and checking words
data Settings = Settings {
	numWordsGen :: Int,
	alphabet :: String
} deriving (Show)

-- | Parse specified file
parseFile :: FilePath -> IO (Settings, [LangDef])
parseFile file = do
	pff <- parseFromFile fileP file
	case pff of
		Right res -> return res
		Left err -> error $ show err

-- | Parser of the whole file
fileP :: Parser (Settings, [LangDef])
fileP = do
	commentsP
	sets <- settingsP
	commentsP
	string "Language definitions:"
	commentsP
	a <- many (commentsP >> langDefP)
	commentsP
	return (sets, sortBy (comparing name) a)

-- | Parser of settings lines
settingsP :: Parser (Settings)
settingsP = do
	manyTill anyChar space
	w_cnt <- many1 digit
	manyTill anyChar (char '{')
	alphabet_s <- manyTill (many (oneOf ", ") >> anyChar) (char '}')
	manyTill anyChar newline
	return Settings{ numWordsGen = read w_cnt, alphabet = alphabet_s }

-- | Parser of any language definition
langDefP :: Parser (LangDef)
langDefP = do
	res <- automatonP <|> regexpP
	return res

-- | Parser of a regexp
regexpP :: Parser (LangDef)
regexpP = do
	char 'R'
	name <- line
	skipMany emptyLine
	value <- line
	return RegExp{name='R':name, value=value}

-- | Parser of an automaton
automatonP :: Parser (LangDef)
automatonP = do
	char 'A'
	name <- manyTill anyChar newline
	alphabet <- manyTill (spaces >> anyChar) newline
	stateLines <- manyTill stateLineP (emptyLine <|> eof)
	return Automaton {
		 name = 'A':name
		,a_alphabet = filter (/= 'e') alphabet
		,states = map fState stateLines
		,startStates = map fState $ filter isStarting stateLines
		,acceptStates = map fState $ filter isAccepting stateLines
		,delta = [(fState sl, mCh, states) | sl <- stateLines, (ch, states) <- zip alphabet (tStates sl), let mCh = if ch=='e' then Nothing else Just ch, states /= ["-"]]
	}


-- | Flag of a state of a FA
data StateFlag = Starting | Accepting deriving (Show, Eq)
-- | StateLine represents a line in a table-defined FA
data StateLine = StateLine {
	 fState :: String
	,fStateFlags :: [StateFlag]
	,tStates :: [[String]]
} deriving Show
isStarting StateLine{fStateFlags = flags} = (Starting `elem` flags)
isAccepting StateLine{fStateFlags = flags} = (Accepting `elem` flags)

-- | Parser of a single state line in table-defined FA
stateLineP :: Parser (StateLine)
stateLineP = do
	(state, flags) <- stateFlagP
	spaces
	statesStr <- manyTill anyChar newline
	return StateLine {
		 fState = state
		,fStateFlags = flags
		,tStates = map (Split.splitOn ",") (Split.splitOn "\t" statesStr)
	}

-- | Parser of a state name and its flag
stateFlagP :: Parser (String, [StateFlag])
stateFlagP = do
{
	spaces;
	parsed <-
		try (do{ char '>'; spaces; state <- many (noneOf "F> \t"); spaces; oneOf "F>"; return (state, [Starting, Accepting])})
	<|>	try (do{ char '>'; spaces; state <- many (noneOf "F> \t"); return (state, [Starting])})
	<|>	try (do{ state <- many (noneOf "F> \t"); spaces; oneOf "F>"; return (state, [Accepting])})
	<|>	do{ state <- many (noneOf "F> \t"); return (state, [])};
	return parsed
}

-- | Parser of comment lines (skip them all)
commentsP :: Parser ()
commentsP = do
{
	skipMany (do {
		char '#';
		manyTill anyChar newline;
		return();
	}
	<|>	emptyLine)
}

-- | Helper parser: skip many spaces (space character or tab); Redefines that one from Parsec
spaces :: Parser ()
spaces = skipMany space

-- | Helper parser: skip one space (space character or tab); Redefines that one from Parsec
space :: Parser (Char)
space = oneOf " \t"

-- | Helper parser: parse the whole line
line :: Parser (String)
line = manyTill anyChar newline

-- | Helper parser: skip empty line
emptyLine :: Parser ()
emptyLine = newline >> return()