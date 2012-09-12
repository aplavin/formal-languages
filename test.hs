import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.HUnit

import qualified DFA
import qualified NFA

dfa = DFA.DFA
    { DFA.states = Set.fromList ['x', 'y']
    , DFA.sigma = Set.fromList [0, 1]
    , DFA.delta = Map.fromList
        [ (('x', 0), 'x')
        , (('y', 0), 'y')
        , (('x', 1), 'y')
        , (('y', 1), 'x') ]
    , DFA.startState = 'x'
    , DFA.acceptStates = Set.fromList ['y'] }

nfa = NFA.NFA
    { NFA.states = Set.fromList [1, 2, 3]
    , NFA.sigma = Set.fromList ['a', 'b']
    , NFA.delta = Map.fromList
        [ ((1, Just 'b'),   Set.fromList [2])
        , ((1, Nothing),    Set.fromList [3])
        , ((2, Just 'a'),   Set.fromList [2, 3])
        , ((2, Just 'b'),   Set.fromList [3])
        , ((3, Just 'a'),   Set.fromList [1]) ]
    , NFA.startState = 1
    , NFA.acceptStates = Set.fromList [1] }

nfa' = NFA.toDFA nfa

dfaTestCase = TestCase (do
    assertEqual "[]" (DFA.accept [] dfa) False
    assertEqual "[1]" (DFA.accept [1] dfa) True
    assertEqual "[0]" (DFA.accept [0] dfa) False
    assertEqual "[0, 1, 1, 0, 1, 1]"
        (DFA.accept [0, 1, 1, 0, 1, 1] dfa) False
    assertEqual "[0, 1, 1, 0, 1, 1, 1]"
        (DFA.accept [0, 1, 1, 0, 1, 1, 1] dfa) True
    assertEqual "[0, 1, 1, 0, 1, 1, 1, 0]"
        (DFA.accept [0, 1, 1, 0, 1, 1, 1, 0] dfa) True)

nfaTestCase = TestCase (do
    assertEqual "[]" (NFA.accept [] nfa) True
    assertEqual "['a']" (NFA.accept ['a'] nfa) True
    assertEqual "['b']" (NFA.accept ['b'] nfa) False
    assertEqual "['b', 'a', 'b']"
        (NFA.accept ['b', 'a', 'b'] nfa) False
    assertEqual "['b', 'a', 'b', 'a']"
        (NFA.accept ['b', 'a', 'b', 'a'] nfa) True
    assertEqual "['b', 'a', 'b', 'a', 'a']"
        (NFA.accept ['b', 'a', 'b', 'a', 'a'] nfa) True)

convertTestCase = TestCase (do
    assertEqual "[]" (DFA.accept [] nfa') True
    assertEqual "['a']" (DFA.accept ['a'] nfa') True
    assertEqual "['b']" (DFA.accept ['b'] nfa') False
    assertEqual "['b', 'a', 'b']"
        (DFA.accept ['b', 'a', 'b'] nfa') False
    assertEqual "['b', 'a', 'b', 'a']"
        (DFA.accept ['b', 'a', 'b', 'a'] nfa') True
    assertEqual "['b', 'a', 'b', 'a', 'a']"
        (DFA.accept ['b', 'a', 'b', 'a', 'a'] nfa') True)

main = runTestTT $ TestList [dfaTestCase, nfaTestCase, convertTestCase]
