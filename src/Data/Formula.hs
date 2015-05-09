module Data.Formula
where

import Prelude hiding (lookup)
import Data.List (nub, groupBy)
import Data.Map (Map, lookup, fromList, toList)
import Control.Applicative ((<$>),(<*>))

data Formula a
    = Atom a
    | Not (Formula a)
    | And (Formula a) (Formula a)
    | Or  (Formula a) (Formula a)
    | Imp (Formula a) (Formula a)
    | Iff (Formula a) (Formula a)
    deriving (Eq, Show, Read)

instance Functor Formula where
    fmap = onAtoms

onAtoms :: (a -> b) -> Formula a -> Formula b
onAtoms fn fm = case fm of
    Atom x -> Atom . fn $ x
    Not sfm -> Not $ onAtoms fn sfm
    And sfm1 sfm2 -> And (onAtoms fn sfm1) (onAtoms fn sfm2)
    Or  sfm1 sfm2 -> Or  (onAtoms fn sfm1) (onAtoms fn sfm2)
    Imp sfm1 sfm2 -> Imp (onAtoms fn sfm1) (onAtoms fn sfm2)
    Iff sfm1 sfm2 -> Iff (onAtoms fn sfm1) (onAtoms fn sfm2)

onFormulas :: (Formula a -> Formula a) -> Formula a -> Formula a
onFormulas fn fm = case fm of
    a@(Atom _) -> fn a
    Not sfm -> Not (fn sfm)
    And sfm1 sfm2 -> And (fn sfm1) (fn sfm2)
    Or  sfm1 sfm2 -> Or  (fn sfm1) (fn sfm2)
    Imp sfm1 sfm2 -> Imp (fn sfm1) (fn sfm2)
    Iff sfm1 sfm2 -> Iff (fn sfm1) (fn sfm2)

overAtoms :: (a -> b -> b) -> Formula a -> b -> b
overAtoms fn fm b = case fm of
    Atom a -> fn a b
    Not sfm -> overAtoms fn sfm b
    And sfm1 sfm2 -> overAtoms fn sfm1 (overAtoms fn sfm2 b)
    Or  sfm1 sfm2 -> overAtoms fn sfm1 (overAtoms fn sfm2 b)
    Imp sfm1 sfm2 -> overAtoms fn sfm1 (overAtoms fn sfm2 b)
    Iff sfm1 sfm2 -> overAtoms fn sfm1 (overAtoms fn sfm2 b)

atoms :: (Eq a) => Formula a -> [a]
atoms = nub . flip (overAtoms (:)) []

type AssgFN = Map Int Bool
type Error = String
type DIMACS = [[Int]]

eval :: AssgFN -> Formula Int -> Maybe Bool
eval assgFN fm = case fm of
    Atom i -> lookup i assgFN
    Not sfm -> e  sfm
    And sfm1 sfm2 -> (&&) <$> (e sfm1) <*> (e sfm2)
    Or  sfm1 sfm2 -> (||) <$> (e sfm1) <*> (e sfm2)
    Imp sfm1 sfm2 -> (<=) <$> (e sfm1) <*> (e sfm2)
    Iff sfm1 sfm2 -> (==) <$> (e sfm1) <*> (e sfm2)
  where e = eval assgFN

domain :: Formula Int -> [AssgFN]
domain = fmap fromList . sequence . groupBy equalAtom . (flip cartProd) [True,False] . atoms
  where
    cartProd a b = (,) <$> a <*> b
    equalAtom = (\x y -> fst x == fst y)

models :: Formula Int -> [AssgFN]
models fm = [ m | m <- domain fm, eval m fm == Just True ]

nonModels :: Formula Int -> [AssgFN]
nonModels fm = [ nm | nm <- domain fm, eval nm fm == Just False ]

twin :: AssgFN -> AssgFN
twin = fmap not

allAnd :: [Formula a] -> Formula a
allAnd = foldr1 And

allOr :: [Formula a] -> Formula a
allOr = foldr1 Or

literals :: AssgFN -> [Formula Int]
literals = fmap lit . toList
        where
          lit (i, True) = Atom i
          lit (i, False) = (Not . Atom) i

cnfLiterals :: Formula Int -> [[Formula Int]]
cnfLiterals = fmap (literals . twin) . nonModels

cnf :: Formula Int -> Formula Int
cnf = allAnd . fmap allOr . cnfLiterals

dimacs :: [[Formula Int]] -> DIMACS
dimacs = (fmap . fmap) fn
  where
    fn (Atom i)       = i
    fn (Not (Atom i)) = -i
    _ = error "dimacs: formula is not a literal"

-- TODO:
--  * is Formula a a monoid?
--  * is Formua a foldable?

