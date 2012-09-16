module FileParser(parseFile, Settings(..)) where

import Text.Parsec hiding (space, spaces)
import Text.Parsec.String
import qualified Data.List.Split as Split
import Automaton

-- | Settings for generating and checking words
data Settings = Settings {
	numWordsGen :: Int,
	regexp :: String
} deriving (Show)

-- | Flag of a state of a FA
data StateFlag = None | Starting | Accepting deriving (Show, Eq)

-- | StateLine represents a line in a table-defined FA
data StateLine = StateLine {
	 fState :: String
	,fStateFlag :: StateFlag
	,tStates :: [[String]]
} deriving Show

-- | Check if the StateLine represents a starting state
isStarting StateLine{fStateFlag = flag} = (flag == Starting)

-- | Check if the StateLine represents an accepting state
isAccepting StateLine{fStateFlag = flag} = (flag == Accepting)

-- | Parse specified file
parseFile :: FilePath -> IO (Settings, [Automaton])
parseFile file = do
{
	pff <- parseFromFile automataP file;
	case pff of
		Right res -> return res
		Left err -> error $ show err
}

-- | Parser of the whole automata file
automataP :: Parser (Settings, [Automaton])
automataP = do
{
	commentsP;
	sets <- settingsP;
	commentsP;
	string "Automata:";
	commentsP;
	a <- many automatonP;
	return (sets, a);
}

-- | Parser of settings lines
settingsP :: Parser (Settings)
settingsP = do
{
	manyTill anyChar space;
	w_cnt <- many1 digit;
	manyTill anyChar newline;
	commentsP;
	manyTill anyChar (char ':');
	space;
	regexp <- manyTill anyChar newline;
	return Settings{ numWordsGen = read w_cnt, regexp = regexp };
}

-- | Parser of a single automaton
automatonP :: Parser (Automaton)
automatonP = do
{
	name <- manyTill anyChar newline;
	alphabet <- manyTill (spaces >> anyChar) newline;
	stateLines <- manyTill stateLineP emptyLine;
	return Automaton {
		 name = name
		,alphabet = filter (/= 'e') alphabet
		,states = map fState stateLines
		,startStates = map fState $ filter isStarting stateLines
		,acceptStates = map fState $ filter isAccepting stateLines
		,delta = [(fState sl, mCh, states) | sl <- stateLines, (ch, states) <- zip alphabet (tStates sl), let mCh = if ch=='e' then Nothing else Just ch, states /= ["-"]]
	};
}

-- | Parser of a single state line in table-defined FA
stateLineP :: Parser (StateLine)
stateLineP = do
{
	(state, flag) <- stateFlagP;
	spaces;
	statesStr <- manyTill anyChar newline;
	return StateLine {
		 fState = state
		,fStateFlag = flag
		,tStates = map (Split.splitOn ",") (Split.splitOn "\t" statesStr)
	};
}

-- | Parser of a state name and its flag
stateFlagP :: Parser (String, StateFlag)
stateFlagP = do
{
	spaces;
	parsed <-
		try (do{ char '>'; spaces; state <- manyTill anyChar space; return (state, Starting)})
	<|>	try (do{ state <- manyTill anyChar space; oneOf "F>"; return (state, Accepting)})
	<|>	do{ state <- manyTill anyChar space; return (state, None)};
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
	<|>	emptyLine);
}

-- | Helper parser: skip many spaces (space character or tab); Redefines that one from Parsec
spaces :: Parser ()
spaces = skipMany space

-- | Helper parser: skip one space (space character or tab); Redefines that one from Parsec
space :: Parser (Char)
space = oneOf " \t"

-- | Helper parser: skip empty line
emptyLine :: Parser ()
emptyLine = newline >> return()