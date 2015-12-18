module NLambda
(
module Nominal.Atom,
module Nominal.Automaton.Base,
module Nominal.Automaton.Deterministic,
module Nominal.Automaton.Nondeterministic,
module Nominal.Conditional,
module Nominal.Contextual,
module Nominal.Either,
module Nominal.Formula,
module Nominal.Graph,
module Nominal.Maybe,
module Nominal.Set,
module Nominal.Type,
module Nominal.Variable,
module Nominal.VariablesSpace,
module Nominal.Variants,
a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
)
where

import Nominal.Atom
import Nominal.Automaton.Base
import Nominal.Automaton.Deterministic
import Nominal.Automaton.Nondeterministic
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Either
import Nominal.Formula
import Nominal.Graph
import Nominal.Maybe
import Nominal.Set
import Nominal.Type (NominalType(eq), neq)
import Nominal.Variable (Variable, variable, variableName)
import Nominal.VariablesSpace
import Nominal.Variants (Variants, iteV, iteV')
import Prelude hiding (or, and, not, sum, map, filter, maybe)

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------
a = atom "a"
b = atom "b"
c = atom "c"
d = atom "d"
e = atom "e"
f = atom "f"
g = atom "g"
h = atom "h"
i = atom "i"
j = atom "j"
k = atom "k"
l = atom "l"
m = atom "m"
n = atom "n"
o = atom "o"
p = atom "p"
q = atom "q"
r = atom "r"
s = atom "s"
t = atom "t"
u = atom "u"
v = atom "v"
w = atom "w"
x = atom "x"
y = atom "y"
z = atom "z"

-- example program

nlProgram = do
    a <- newAtom
    b <- newAtom
    return $ let set = insert a atoms
             in insert b set


-- graph

gr = atomsGraph $ filter (\(x,y) -> eq x a \/ eq y a) atomsPairs
gIn = atomsGraph $ filter (eq a . snd) atomsPairs
gOut = atomsGraph $ filter (eq a . fst) atomsPairs
gAB = addEdge (a,b) emptyGraph
bigraph = atomsGraph $ filter (\(x,y) -> (lt x a /\ lt y a) \/ (gt x a /\ gt y a)) atomsPairs
bigraphMonotonic = atomsGraph $ filter (\(x,y) -> (lt x y) /\ ((lt x a /\ lt y a) \/ (gt x a /\ gt y a))) atomsPairs

-- auto

f1 = le a b
f2 = le b c
f3 = le c d
f4 = le d e

result = simplify $ accepts (atomsDA (fromList [a,b,c]) (\x y -> ite (eq x y) a b) a (singleton c)) [a,b,c]
-- ((a /= b || a /= c) && (b /= c || a = b) && b = c) || (((a /= b && b = c) || (a = b && a = c)) && a = c)
-- (((a /= b && b = c) || (a = b && a = c)) && a = c) || (((a /= b || a /= c) && (b /= c || a = b)) && b = c)
result1 = eq b c /\ (neq a b \/ neq a c) /\ (neq b c \/ eq a b) -- -> false

createToMinAuto n = atomsDA (replicateAtomsUntil n) (flip (:)) [] (filter (\(a:l) -> or $ fmap (eq a) l) $ replicateAtoms n)
toMinAuto = createToMinAuto 2
parityAuto = atomsDA (fromList [0,1]) (\q _ -> mod (succ q) 2) 0 (singleton 0) :: Automaton Int Atom

result2 = simplify $ equivalentDA (differenceDA toMinAuto parityAuto) toMinAuto
result3 = simplify $ equivalentDA (differenceDA parityAuto toMinAuto) parityAuto
result4 = simplify $ equivalentDA parityAuto (unionDA parityAuto toMinAuto)
result5 = simplify $ minimize toMinAuto
gg = graph (square (states toMinAuto)) (map (\(s1,_,s2) -> (s1,s2)) $ pairsDelta (delta toMinAuto) (delta toMinAuto))

--data State a = Init | One a | Two a a | Final | NotFinal deriving (Eq, Ord, Show)
--instance NominalType a => NominalType (State a) where
--    eq Init Init = true
--    eq (One a) (One b) = eq a b
--    eq (Two a1 a2) (Two b1 b2) = eq (a1,a2) (b1,b2)
--    eq Final Final = true
--    eq NotFinal NotFinal = true
--    eq _ _ = false
--    mapVariables f Init = Init
--    mapVariables f Final = Final
--    mapVariables f NotFinal = NotFinal
--    mapVariables f (One a) = One (mapVariables f a)
--    mapVariables f (Two a b) = Two (mapVariables f a) (mapVariables f b)
--    foldVariables f acc (One a) = foldVariables f acc a
--    foldVariables f acc (Two a b) = foldVariables f acc (a, b)
--    foldVariables _ acc _  = acc
--
--qs = unions [singleton Init, map One atoms, pairsWith Two atoms atoms, singleton Final]
--tr :: State Atom -> Atom -> State Atom
--tr Init a = One a
--tr (One a) b = Two a b
--tr (Two a b) c = ite (eq a c \/ eq b c) Final NotFinal
--
--testMinAut = atomsDA qs tr
