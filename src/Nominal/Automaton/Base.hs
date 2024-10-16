{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Nominal.Automaton.Base where

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

import Debug.Trace

----------------------------------------------------------------------------------------------------
-- Definition of automaton
----------------------------------------------------------------------------------------------------

-- | An automaton with a set of state with type __q__ accepting\/rejecting words from an alphabet with type __a__.
data Automaton q a = Automaton {states :: Set q, alphabet :: Set a, delta :: Set (q, a, q),
                                initialStates :: Set q, finalStates :: Set q}
  deriving (Eq, Ord, Read, Generic, Nominal, Contextual, Conditional)

instance (Nominal q, Show q, Nominal a, Show a) => Show (Automaton q a) where
    show (Automaton q a d i f) =
      "Automaton {\n" ++
      "  States: {\n" ++
           concatMap (\s -> "    " ++ s ++ ",\n") (showVerbose q) ++
      "  },\n" ++
      "  Alphabet: " ++ show a ++ ",\n" ++
      "  Delta: {\n" ++
           concatMap (\s -> "    " ++ s ++ ",\n") (showVerbose d) ++
      "  },\n" ++
      "  Intial States: {\n" ++
           concatMap (\s -> "    " ++ s ++ ",\n") (showVerbose i) ++
      "  },\n" ++
      "  Final States: {\n" ++
           concatMap (\s -> "    " ++ s ++ ",\n") (showVerbose f) ++
      "  }\n" ++
      "}\n"


-- | An automaton constructor.
automaton :: (Nominal q, Nominal a) => Set q -> Set a -> Set (q, a, q) -> Set q -> Set q -> Automaton q a
automaton = Automaton

-- | An automaton constructor with additional not accepting state.
automatonWithTrashCan :: (Nominal q, Nominal a) => Set q -> Set a -> Set (q, a, q) -> Set q -> Set q -> Automaton (Maybe q) a
automatonWithTrashCan q a d i f = onlyReachable $ Automaton q' a (d1 `union` d2) i' f'
    where q' = insert Nothing $ map Just q
          d1 = mapFilter (\(s1,l,s2) -> maybeIf (contains q s1 /\ contains q s2) (Just s1,l,Just s2)) d
          d2 = map (\(s1,l) -> (s1,l,Nothing)) (pairs q' a \\ map (\(s,l,_)-> (s,l)) d1)
          i' = map Just (intersection q i)
          f' = map Just (intersection q f)

----------------------------------------------------------------------------------------------------
-- Automaton functions
----------------------------------------------------------------------------------------------------

transitFromStates :: (Nominal q, Nominal a) => Automaton q a -> (q -> Formula) -> a -> Set q
transitFromStates aut cf l = mapFilter (\(s1, l', s2) -> maybeIf (cf s1 /\ eq l l') s2) (delta aut)

transit :: (Nominal q, Nominal a) => Automaton q a -> q -> a -> Set q
transit aut s = transitFromStates aut (eq s)

transitSet :: (Nominal q, Nominal a) => Automaton q a -> Set q -> a -> Set q
transitSet aut ss = transitFromStates aut (contains ss)

-- | Checks whether an automaton accepts a word.
-- FIXME: I am almost sure there is a bug here. `accepts [a,a] = a != 4 but accepts [0,0] = a₂ = 0 ∧ a₂ ≠ 4`??
accepts :: (Nominal q, Nominal a) => Automaton q a -> [a] -> Formula
accepts aut s = finalStates aut `intersect` reached
    where reached = foldl (transitSet aut) (initialStates aut) s

boolAccepts :: (Nominal q, Nominal i) => Automaton q i -> [i] -> Bool
boolAccepts aut s = if Prelude.not (isTrue accs || isFalse accs)
    then error "@@@ Error: accepts must be bolean on concrete input! @@@"
    else isTrue accs
    where
        accs = accepts aut s

transitionGraph :: (Nominal q, Nominal a) => Automaton q a -> Graph q
transitionGraph aut = graph (states aut) (map (\(s1, _, s2) -> (s1, s2)) $ delta aut)

onlyReachable :: (Nominal q, Nominal a) => Automaton q a -> Automaton q a
onlyReachable aut@(Automaton q a d i f) = Automaton q' a d' i (intersection q' f)
    where q' = reachableFromSet (transitionGraph aut) (initialStates aut)
          d' = mapFilter (\(s1,l,s2) -> maybeIf (contains q' s1) (s1,l,s2)) d

-- | Checks whether an automaton accepts any word.
isNotEmptyAutomaton :: (Nominal q, Nominal a) => Automaton q a -> Formula
isNotEmptyAutomaton = isNotEmpty . finalStates . onlyReachable

-- | Checks whether an automaton rejects all words.
isEmptyAutomaton :: (Nominal q, Nominal a) => Automaton q a -> Formula
isEmptyAutomaton = Nominal.Formula.not . isNotEmptyAutomaton

pairsDelta :: (Nominal q1, Nominal q2, Nominal a) => Set (q1,a,q1) -> Set (q2,a,q2) -> Set ((q1,q2),a,(q1,q2))
pairsDelta = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> maybeIf (eq l l') ((s1,s1'),l,(s2,s2')))

-- | Returns an automaton that accepts the union of languages accepted by two automata.
unionAutomaton :: (Nominal q1, Nominal q2, Nominal a) => Automaton q1 a -> Automaton q2 a -> Automaton (Either q1 q2) a
unionAutomaton (Automaton q1 a d1 i1 f1) (Automaton q2 _ d2 i2 f2) = Automaton q a d i f
    where eitherUnion s1 s2 = map Left s1 `union` map Right s2
          q = eitherUnion q1 q2
          d = map (\(s1,l,s2)->(Left s1,l,Left s2)) d1 `union` map (\(s1,l,s2)->(Right s1,l,Right s2)) d2
          i = eitherUnion i1 i2
          f = eitherUnion f1 f2

-- | Returns an automaton that accepts the intersection of languages accepted by two automata.
intersectionAutomaton :: (Nominal q1, Nominal q2, Nominal a) => Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
intersectionAutomaton (Automaton q1 a d1 i1 f1) (Automaton q2 _ d2 i2 f2) = Automaton q a d i f
    where q = pairs q1 q2
          d = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> maybeIf (eq l l') ((s1, s1'),l,(s2, s2'))) d1 d2
          i = pairs i1 i2
          f = pairs f1 f2

----------------------------------------------------------------------------------------------------
-- Automaton minimization
----------------------------------------------------------------------------------------------------

-- | Returns a minimal automaton accepting the same language as a given automaton.
minimize :: (Nominal q, Nominal a) => Automaton q a -> Automaton (Set q) a
minimize aut@(Automaton q a d i f) = automaton q' a d' i' f'
    where relGraph = graph (square q) (map (\(s1,_,s2) -> (s1,s2)) $ pairsDelta d d)
          nf = q \\ f
          equiv = square q \\ reachableFromSet (reverseEdges relGraph) (pairs nf f `union` pairs f nf)
          q' = map vertices (stronglyConnectedComponents $ graph q equiv)
          d' = filter (\(ss1,ll,ss2) -> exists (\(s1,l,s2)-> member s1 ss1 /\ eq l ll /\ member s2 ss2) d) (triples q' a q')
          i' = filter (intersect i) q'
          f' = filter (intersect f) q'
