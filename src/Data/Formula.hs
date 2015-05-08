module Data.Formula
where

import Prelude hiding (lookup)
import Data.List (nub)
import Data.Map (Map, lookup)
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

eval :: AssgFN -> Formula Int -> Maybe Bool
eval assgFN fm = case fm of
    Atom i -> lookup i assgFN
    Not sfm -> e  sfm
    And sfm1 sfm2 -> (&&) <$> (e sfm1) <*> (e sfm2)
    Or  sfm1 sfm2 -> (||) <$> (e sfm1) <*> (e sfm2)
    Imp sfm1 sfm2 -> (<=) <$> (e sfm1) <*> (e sfm2)
    Iff sfm1 sfm2 -> (==) <$> (e sfm1) <*> (e sfm2)
  where e = eval assgFN



-- TODO:
--  * is Formula a a monoid?
--  * is Formua a foldable?

