{-# LANGUAGE DefaultSignatures, FlexibleContexts, CPP, TypeOperators #-}

module Nominal.Type where

import Data.Map (Map, findWithDefault)
import Data.Set (Set, empty, insert)
import Nominal.Formula
import Nominal.Formula.Operators (foldFormulaVariables, mapFormulaVariables)
import Nominal.Variable (Variable)
import Nominal.Variants (Variants, fromList, map, prod, toList, variant, variantsRelation)
import Prelude hiding (and, map, not, or)
import GHC.Generics

----------------------------------------------------------------------------------------------------
-- Nominal
----------------------------------------------------------------------------------------------------

-- | Variables scope.
data Scope
    -- | All variables.
    = All
    -- | Only free variables.
    | Free

-- | Map function for variables from a scope.
type MapVarFun = (Scope, Variable -> Variable)
-- | Fold function for variables from a scope.
type FoldVarFun b = (Scope, Variable -> b -> b)

-- | Basic type in 'NLambda' required by most of functions in the module.
-- The Ord instance is used for efficiency.
-- By using generics, one can derive instances of this class for custom
-- data types, like this:
--
-- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- > import GHC.Generics (Generic)
-- >
-- > data Foo = Foo1 Atom [Atom] | Foo2 (Set Atom)
-- >   deriving (Eq, Ord, Generic, Nominal)
class Ord a => Nominal a where
    -- | Checks equivalence of two given elements.
    eq :: a -> a -> Formula
    -- | If __a__ is a variant type then returns variants values.
    variants :: a -> Variants a
    -- | Map all variables from a given scope.
    mapVariables :: MapVarFun -> a -> a
    -- | Fold all variables form a given scope.
    foldVariables :: FoldVarFun b -> b -> a -> b

    -- We give default implementation if the type is generic.
    default eq :: (Generic a, GNominal (Rep a)) => a -> a -> Formula
    eq x y = geq (from x) (from y)
    default variants :: (Generic a, GNominal (Rep a)) => a -> Variants a
    variants x = fromList . gvmap to $ gvariants (from x)
    default mapVariables :: (Generic a, GNominal (Rep a)) => MapVarFun -> a -> a
    mapVariables f x = to (gmapVariables f (from x))
    default foldVariables :: (Generic a, GNominal (Rep a)) => FoldVarFun b -> b -> a -> b
    foldVariables f b x = gfoldVariables f b (from x)

-- | Checks whether two elements are not equivalent.
neq :: Nominal a => a -> a -> Formula
neq x1 x2 = not $ eq x1 x2

----------------------------------------------------------------------------------------------------
-- Operations on all variables
----------------------------------------------------------------------------------------------------

collectWith :: (Nominal a, Ord b) => (Variable -> Maybe b) -> a -> Set b
collectWith cf = foldVariables (All, maybe id insert . cf) empty

getAllVariables :: Nominal a => a -> Set Variable
getAllVariables = foldVariables (All, insert) empty

freeVariables :: Nominal a => a -> Set Variable
freeVariables = foldVariables (Free, insert) empty

mapVariablesIf :: Nominal a => (Variable -> Bool) -> (Variable -> Variable) -> a -> a
mapVariablesIf cf mf = mapVariables (All, \v -> if cf v then mf v else v)

replaceVariables :: Nominal a => Map Variable Variable -> a -> a
replaceVariables varsMap = mapVariables (All, \var -> findWithDefault var var varsMap)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance Nominal Variable where
    eq = equals
    variants = variant
    mapVariables (_, f) = f
    foldVariables (_, f) acc v = f v acc

instance Nominal Formula where
    eq = iff
    variants = variant
    mapVariables (_, f) = mapFormulaVariables f
    foldVariables (_, f) = foldFormulaVariables f

instance Nominal a => Nominal (Variants a) where
    eq = variantsRelation eq
    variants = map variant
    mapVariables f = fromList . mapVariables f . toList
    foldVariables f acc = foldl (foldVariables f) acc . toList

-- We will define trivial instances (i.e. where the action on Atoms does
-- not do anything at all). We could spell this out for each type, but
-- I am lazy, so I defined a macro. There are other ways to do this,
-- but this is a pretty obvious and readable one. This used to be the default
-- of Nominal, but I think it's better to make it explicit (i.e. not
-- default)
#define NOM_INSTANCE(foo)         \
instance Nominal foo where;   \
    eq x y = fromBool (x == y);   \
    variants = variant;           \
    mapVariables _ = id;          \
    foldVariables _ acc _ = acc;  \

-- All of these are nominal types with the trivial action
NOM_INSTANCE(Bool)
NOM_INSTANCE(Char)
NOM_INSTANCE(Double)
NOM_INSTANCE(Float)
NOM_INSTANCE(Int)
NOM_INSTANCE(Integer)
NOM_INSTANCE(Ordering)

-- Here are some generic instances. We do not have to provide any
-- implementation as they are dictated by their algebraic data type.
instance Nominal ()
instance Nominal a => Nominal [a]
instance Nominal a => Nominal (Maybe a)
instance (Nominal a, Nominal b) => Nominal (Either a b)
instance (Nominal a, Nominal b) => Nominal (a, b)
instance (Nominal a, Nominal b, Nominal c) => Nominal (a, b, c)
instance (Nominal a, Nominal b, Nominal c, Nominal d) => Nominal (a, b, c, d)
instance (Nominal a, Nominal b, Nominal c, Nominal d, Nominal e) => Nominal (a, b, c, d, e)
instance (Nominal a, Nominal b, Nominal c, Nominal d, Nominal e, Nominal f) => Nominal (a, b, c, d, e, f)
instance (Nominal a, Nominal b, Nominal c, Nominal d, Nominal e, Nominal f, Nominal g) => Nominal (a, b, c, d, e, f, g)

-- The generic interface on the functor level
-- Note that we do not require an ordering, because that is not easy
-- to do on the functor level. To represet Variants here, we use a
-- simple list data type, then in the Nominal implementation it
-- will be put in a Data.Map for efficiency. This is equivalent to
-- the previous implementation (since I was using monotonic functions)
class GNominal f where
    geq :: f a -> f a -> Formula
    gvariants :: f a -> GVariants (f a)
    gmapVariables :: MapVarFun -> f a -> f a
    gfoldVariables :: FoldVarFun b -> b -> f a -> b

type GVariants a = [(a, Formula)]

gvmap :: (a -> b) -> GVariants a -> GVariants b
gvmap f ls = [(f x, form) | (x, form) <- ls]

-- We will define an instance for 0, 1, +, * and constants (on the type
-- level). Then we can derive instances for any algebraic datatype.
-- The instances for Formula and Variant are special cases, and are
-- implemented by hand above.

-- For the void type (no constructor). This is a bit awkward, but valid.
instance GNominal V1 where
    geq _ _ = true
    gvariants x = [] -- void does not have any inhabitants
    gmapVariables _ = id
    gfoldVariables _ acc _ = acc

-- For the unit type (constructors without fields)
instance GNominal U1 where
    geq _ _ = true
    gvariants x = [(x, true)]
    gmapVariables _ = id
    gfoldVariables _ acc _ = acc

-- For constants
instance Nominal c => GNominal (K1 i c) where
    geq (K1 a) (K1 b) = eq a b
    gvariants (K1 a) = gvmap K1 . toList $ variants a
    gmapVariables f (K1 a) = K1 $ mapVariables f a
    gfoldVariables f b (K1 a) = foldVariables f b a

-- For constructors with meta information (which we ignore)
instance GNominal a => GNominal (M1 i c a) where
    geq (M1 a) (M1 b) = geq a b
    gvariants (M1 a) = gvmap M1 $ gvariants a
    gmapVariables f (M1 a) = M1 $ gmapVariables f a
    gfoldVariables f b (M1 a) = gfoldVariables f b a

-- For sums
instance (GNominal a, GNominal b) => GNominal (a :+: b) where
    geq (L1 a) (L1 b) = geq a b
    geq (R1 a) (R1 b) = geq a b
    geq _ _ = false
    gvariants (L1 a) = gvmap L1 $ gvariants a
    gvariants (R1 a) = gvmap R1 $ gvariants a
    gmapVariables f (L1 a) = L1 $ gmapVariables f a
    gmapVariables f (R1 a) = R1 $ gmapVariables f a
    gfoldVariables f b (L1 a) = gfoldVariables f b a
    gfoldVariables f b (R1 a) = gfoldVariables f b a

-- For products
instance (GNominal a, GNominal b) => GNominal (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 /\ geq b1 b2
    gvariants (a :*: b) = [(x1 :*: x2, f1 /\ f2) | (x1, f1) <- gvariants a, (x2, f2) <- gvariants b]
    gmapVariables f (a :*: b) = gmapVariables f a :*: gmapVariables f b
    gfoldVariables f c (a :*: b) = gfoldVariables f (gfoldVariables f c a) b
