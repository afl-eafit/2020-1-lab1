{- | This module is used to represent PushDown Automata. -}
module PushDownAutomata where

import Data.Map as Map
import Data.Set as Set

{- | Represents the Transition Function (delta function) of a
     PushDownAutomata.  The map pairs a tuple of a state, a symbol and
     a stack symbol with a tuple that has the new state and a list of
     stack symbols. The symbol in the first tuple is represented by a
     list because, it can be either an input symbol or the empty
     string (see 6.1.2, definition of PDA)-}
type TransitionFunction state symbol symbolp
     = Map (state, [symbol], symbolp) (Set (state, [symbolp]))

{- | Represents a PushDown Automaton (PDA).  The type can represent a
     deterministic and non-deterministic PDA, with states of type
     @state@, input symbols of type @symbol@ and stack symbols of type
     @symbolp@ -}
data PushDownAutomata state symbol symbolp = PDA
  {
      -- | The set of states.
      states        :: Set state
      -- | The set of input symbols.
    , alphabet      :: Set symbol
      -- | The set of stack symbols
    , stackalphabet :: Set symbolp
      -- | The Transition Function.
    , delta        :: TransitionFunction state symbol symbolp
      -- | The initial state.
    , initialState :: state
      -- | The start symbol
    , z0           :: symbolp
      -- | The set of final or accepting states.
    , acceptState  :: Set state
  }

{- | Adds a transition to the given TransitionFunction.  @(state,
     [symbol], symbolp, state, [symbolp])@ is a tuple whose elements
     represent the origin state, the symbol that executes the
     transition, the last symbol of the stack, the final state and the
     string to replace the stack symbol respectively.-}
addTransition :: (Ord state, Ord symbol, Ord symbolp) =>
                 (state, [symbol], symbolp, state, [symbolp]) ->
                 TransitionFunction state symbol symbolp ->
                 TransitionFunction state symbol symbolp
addTransition (q, s, sp, q', y) = Map.insertWith Set.union (q, s, sp)
                                  (Set.singleton (q', y))

-- | Formats how to display instances of PushdownAutomata.
instance (Show state, Show symbol, Show symbolp) =>
          Show (PushDownAutomata state symbol symbolp) where
  show (PDA st sy ssy tf is z0 ac) =
       "States:         "   ++ show st ++
       "\nAlphabet:       " ++ show sy ++
       "\nStack Alphabet  " ++ show ssy ++
       "\nDelta:          " ++ show tf ++
       "\nInitial States: " ++ show is ++
       "\nStart symbol    " ++ show z0 ++
       "\nAccept States:  " ++ show ac

{- | First automata example. This PDA represents the following language,
     L(automata0) = {ww^r | w in [0, 1]*} with w^r being the reversed
     word. This is Example 6.2 of the guide book. -}
automata0 :: PushDownAutomata Int Int Int
automata0 = PDA {
      states        = Set.fromList [0, 1, 2]
    , alphabet      = Set.fromList [0, 1]
    , stackalphabet = Set.fromList [0, 1, -1]
    , delta         = delta'
    , initialState  = 0
    , z0            = -1
    , acceptState   = Set.fromList [2]
  } where delta' :: TransitionFunction Int Int Int
          delta' = addTransition (0, [0], -1, 0, [0, -1]) $
                   addTransition (0, [1], -1, 0, [1, -1]) $
                   addTransition (0, [0], 0, 0, [0, 0]) $
                   addTransition (0, [0], 1, 0, [0, 1]) $
                   addTransition (0, [1], 0, 0, [1, 0]) $
                   addTransition (0, [1], 1, 0, [1, 1]) $
                   addTransition (0, [], -1, 1, [-1]) $
                   addTransition (0, [], 0, 1, [0]) $
                   addTransition (0, [], 1, 1, [1]) $
                   addTransition (1, [0], 0, 1, []) $
                   addTransition (1, [1], 1, 1, []) $
                   addTransition (1, [], -1, 2, [-1])
                   Map.empty

{- | Second automata example. This PDA represents the following language,
     L(automata1) = {a^nb^n | n >= 1}.-}
automata1 :: PushDownAutomata Int Char Char
automata1 = PDA {
      states        = Set.fromList [0, 1, 2]
    , alphabet      = Set.fromList ['a', 'b']
    , stackalphabet = Set.fromList ['a', 'b', 'z']
    , delta         = delta'
    , initialState  = 0
    , z0            = 'z'
    , acceptState   = Set.fromList [2]
  } where delta' :: TransitionFunction Int Char Char
          delta' = addTransition (0, ['a'], 'z', 0, ['a', 'z']) $
                   addTransition (0, ['a'], 'a', 0, ['a', 'a']) $
                   addTransition (0, ['b'], 'a', 1, []) $
                   addTransition (1, ['b'], 'a', 1, []) $
                   addTransition (1, [], 'z', 2, ['z'])
                   Map.empty

{- | Third automata example. This PDA represents the following language.
     L(automata1) = {a^(2n)b^(3n) | n >= 1}.-}
automata2 :: PushDownAutomata Int Char Char
automata2 = PDA {
      states        = Set.fromList [0, 1, 2, 3, 4]
    , alphabet      = Set.fromList ['a', 'b']
    , stackalphabet = Set.fromList ['a', 'b', 'z']
    , delta         = delta'
    , initialState  = 0
    , z0            = 'z'
    , acceptState   = Set.fromList [4]
  } where delta' :: TransitionFunction Int Char Char
          delta' = addTransition (0, ['a'], 'z', 1, ['z']) $
                   addTransition (1, ['a'], 'z', 2, ['a', 'a', 'a', 'z']) $
                   addTransition (1, ['a'], 'a', 2, ['a', 'a', 'a', 'a']) $
                   addTransition (2, ['a'], 'a', 1, ['a']) $
                   addTransition (2, ['b'], 'a', 3, []) $
                   addTransition (3, ['b'], 'a', 3, []) $
                   addTransition (3, [], 'z', 4, ['z'])
                   Map.empty
