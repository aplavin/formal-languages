import System.IO
import Text.Regex.Posix
import Text.Parsec hiding (space, spaces)
import Text.Parsec.String
import qualified Data.List.Split as Split
import Data.List
import Automaton
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DFA
import qualified NFA

data Settings = Settings {
	numWordsGen :: Int,
	regexp :: String
} deriving (Show)

space :: Parser (Char)
space = oneOf " \t"

spaces :: Parser ()
spaces = skipMany space

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

emptyLine :: Parser ()
emptyLine = newline >> return()

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

data StateFlag = None | Starting | Accepting deriving (Show, Eq)
data StateLine = StateLine {
	 fState :: String
	,fStateFlag :: StateFlag
	,tStates :: [[String]]
} deriving Show

isStarting StateLine{fStateFlag = Starting} = True
isStarting _ = False

isAccepting StateLine{fStateFlag = Accepting} = True
isAccepting _ = False

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

automatonP :: Parser (Automaton)
automatonP = do
{
	name <- manyTill anyChar newline;
	alphabet <- manyTill (spaces >> anyChar) newline;
	stateLines <- manyTill stateLineP emptyLine;
	return Automaton {
		 name = name
		,alphabet = alphabet
		,states = map fState stateLines
		,startStates = map fState $ filter isStarting stateLines
		,acceptStates = map fState $ filter isAccepting stateLines
		,delta = [(fState sl, ch, states) | sl <- stateLines, (ch, states) <- zip alphabet (tStates sl)]
	};
}

automataP :: Parser (Settings, Automaton)
automataP = do
{
	commentsP;
	sets <- settingsP;
	commentsP;
	string "Automata:";
	commentsP;
	a <- automatonP;
	return (sets, a);
}

main = do
	pff <- parseFromFile automataP "automata.txt"
	putStrLn $ case pff of
		Right res -> show $ snd res
		Left err -> show err