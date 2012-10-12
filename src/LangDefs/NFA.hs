module LangDefs.NFA(NFA (..), accept, toDFA) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import qualified LangDefs.DFA as DFA

type Delta s a = Map.Map (s, Maybe a) (Set.Set s)

data NFA s a = NFA
    { states :: Set.Set s
    , sigma :: Set.Set a
    , delta :: Delta s a
    , startState :: s
    , acceptStates :: Set.Set s
    } deriving (Show, Eq)

move :: (Ord s, Ord a) => s -> Maybe a -> NFA s a -> Set.Set s
move state alpha nfa =
    fromMaybe Set.empty result
    where result = Map.lookup (state, alpha) $ delta nfa

moveWithAlpha :: (Ord s, Ord a) => s -> a -> NFA s a -> Set.Set s
moveWithAlpha state alpha = move state (Just alpha)

moveWithNothing :: (Ord s, Ord a) => s -> NFA s a -> Set.Set s
moveWithNothing state = move state Nothing

epsilonClosure :: (Ord s, Ord a) => Set.Set s -> NFA s a -> Set.Set s
epsilonClosure states nfa = Set.fold appendStates states states
    where appendStates s acc = Set.union acc $ moveWithNothing s nfa

trans :: (Ord s, Ord a) => Set.Set s -> a -> NFA s a -> Set.Set s
trans states alpha nfa = Set.fold appendStates Set.empty states
    where appendStates s acc = Set.union acc $ move s
          move s = epsilonClosure (moveWithAlpha s alpha nfa) nfa

run :: (Ord s, Ord a) => [a] -> NFA s a -> Set.Set s
run input nfa = foldl trans' startStates input
    where trans' states alpha = trans states alpha nfa
          startStates = epsilonClosure (Set.singleton $ startState nfa) nfa

accept :: (Ord s, Ord a) => [a] -> NFA s a -> Bool
accept input nfa = not $ Set.null $ Set.intersection final $ acceptStates nfa
    where final = run input nfa

powerset :: (Ord s) => Set.Set s -> Set.Set (Set.Set s)
powerset set = Set.fold union emptySet set
    where union s acc = Set.union acc $ Set.map (Set.insert s) acc
          emptySet = Set.singleton Set.empty

toDFA :: (Ord s, Ord a) => NFA s a -> DFA.DFA (Set.Set s) a
toDFA nfa = DFA.DFA
    { DFA.states = states'
    , DFA.sigma = sigma'
    , DFA.delta = delta'
    , DFA.startState = starts
    , DFA.acceptStates = Set.filter isAccept states' }
    where states' = powerset $ states nfa
          sigma' = sigma nfa
          delta' = Map.fromList $ foldl run' [] stateList
              where run' acc s = foldl (\acc' a -> trans' s a : acc') acc sigmaList
                    trans' state alpha = ((state, alpha), trans state alpha nfa)
                    sigmaList = Set.toList sigma'
                    stateList = Set.toList states'
          starts = epsilonClosure (Set.singleton $ startState nfa) nfa
          isAccept = not . Set.null . Set.intersection (acceptStates nfa)
