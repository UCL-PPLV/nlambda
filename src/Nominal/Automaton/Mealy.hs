{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Nominal.Automaton.Mealy where

import Nominal.Conditional
import Nominal.Contextual
import Nominal.Formula
import Nominal.Graph
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variants (variant)
import Prelude hiding (filter, map)
import GHC.Generics (Generic)

import Data.Graph (path)
import Data.List hiding (map, singleton)
import Data.Maybe (fromJust)

----------------------------------------------------------------------------------------------------
-- Definition of Mealy Machine
----------------------------------------------------------------------------------------------------

data Mealy q i o = Mealy {mealyStates :: Set q, initialState :: q, inputAlpha :: Set i, outputAlpha :: Set o, mealyDelta :: Set (q, i, o, q)}
  deriving (Eq, Ord, Read, Generic, Nominal, Contextual, Conditional)

instance (Nominal q, Show q, Nominal i, Show i, Nominal o, Show o) => Show (Mealy q i o) where
    show (Mealy q s i o d) =
      "Mealy {\n" ++
      "  States: {\n" ++
           concatMap (\s -> "    " ++ s ++ ",\n") (showVerbose q) ++
      "  },\n" ++
      "  Input Alphabet: " ++ show i ++ ",\n" ++
      "  Output Alphabet: " ++ show o ++ ",\n" ++
      "  Delta: {\n" ++
           concatMap (\s -> "    " ++ s ++ ",\n") (showVerbose d) ++
      "  },\n" ++
      "  Intial State: " ++ show s ++ "\n" ++
      "}\n"

-- | An automaton constructor.
mealy :: (Nominal q, Nominal i, Nominal o) => Set q -> q -> Set i -> Set o -> Set (q, i, o, q) -> Mealy q i o
mealy = Mealy

----------------------------------------------------------------------------------------------------
-- Automaton functions
----------------------------------------------------------------------------------------------------

transitFromStates :: (Nominal q, Nominal i, Nominal o) => Mealy q i o -> (q -> Formula) -> i -> Set (q, o)
transitFromStates aut cf l = mapFilter (\(s1, li, lo, s2) -> maybeIf (cf s1 /\ eq l li) (s2, lo)) (mealyDelta aut)

transit :: (Nominal q, Nominal i, Nominal o) => Mealy q i o -> q -> i -> Set (q, o)
transit aut s = transitFromStates aut (eq s)

transitSet :: (Nominal q, Nominal i, Nominal o) => Mealy q i o -> Set q -> i -> Set (q, o)
transitSet aut ss = transitFromStates aut (contains ss)

output :: (Nominal q, Nominal i, Nominal o, Show q, Show i, Show o) => Mealy q i o -> [i] -> Set o
output aut [] = empty
output aut input = outs
    where
        (prefix, lst) = (init input, last input)
        path = foldl (\qs i -> map fst (transitSet aut qs i)) (singleton $ initialState aut) prefix
        outs = map snd $ transitSet aut path lst
