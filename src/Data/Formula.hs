module Data.Formula
where

import Prelude hiding (lookup)

import Control.Applicative ((<$>),(<*>))

import Data.Word (Word)
import Data.List (nub, groupBy)
import Data.Map  (Map, lookup, fromList, toList)
import Data.Bits (shift, setBit)

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

type VarID  = Word
type AssgFN = Map Word Bool
type Error  = String
type DIMACS = [[Int]]

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

eval :: AssgFN -> Formula VarID -> Maybe Bool
eval assgFN fm = case fm of
    Atom w -> lookup w assgFN
    Not sfm -> not <$> (e sfm)
    And sfm1 sfm2 -> (&&) <$> (e sfm1) <*> (e sfm2)
    Or  sfm1 sfm2 -> (||) <$> (e sfm1) <*> (e sfm2)
    Imp sfm1 sfm2 -> (<=) <$> (e sfm1) <*> (e sfm2)
    Iff sfm1 sfm2 -> (==) <$> (e sfm1) <*> (e sfm2)
  where
    e = eval assgFN



domain :: Formula VarID -> [AssgFN]
domain = fmap fromList . sequence . groupBy equalAtom . (flip cartProd) [True,False] . atoms
  where
    cartProd a b = (,) <$> a <*> b
    equalAtom = (\x y -> fst x == fst y)

models :: Formula VarID -> [AssgFN]
models fm = [ m | m <- domain fm, eval m fm == Just True ]

nonModels :: Formula VarID -> [AssgFN]
nonModels fm = [ nm | nm <- domain fm, eval nm fm == Just False ]

twin :: AssgFN -> AssgFN
twin = fmap not

allAnd :: [Formula a] -> Formula a
allAnd = foldr1 And

allOr :: [Formula a] -> Formula a
allOr = foldr1 Or

literals :: AssgFN -> [Formula VarID]
literals = fmap lit . toList
        where
          lit (i, True) = Atom i
          lit (i, False) = (Not . Atom) i

cnfLiterals :: Formula VarID -> [[Formula VarID]]
cnfLiterals fm = case nonModels fm of
    [] -> fmap (\w -> [Atom w, (Not . Atom) w]) . atoms $ fm
    nms -> fmap (literals .twin) $ nms

cnf :: Formula VarID -> Formula VarID
cnf = allAnd . fmap allOr . cnfLiterals

-- ------------------------------------------------------------------------------------
--tseitin :: Formula VarID -> Formula VarID

tseitin = fmap cnf . bicond

bicond fm =
    let offset = (maximum . atoms) fm
    in  bicond' offset 1 fm

bicond' offset n fm = case fm of
    Atom _ -> []
    Not sfm1 ->
        let newID = n + offset
        in  case sfm1 of
                a@(Atom i) -> [ Iff (Atom newID) (Not a)]
                _ -> let nSfm1 = enumL n
                     in  [ Iff (Atom newID) (Not (Atom nSfm1)) ] ++ bicond' offset nSfm1 sfm1
    And sfm1 sfm2 -> binFunc n (And,sfm1,sfm2)
    Or  sfm1 sfm2 -> binFunc n (Or ,sfm1,sfm2)
    Imp sfm1 sfm2 -> binFunc n (Imp,sfm1,sfm2)
    Iff sfm1 sfm2 -> binFunc n (Iff,sfm1,sfm2)
  where
    binFunc n (f, sfm1, sfm2) =
        let newID = n + offset
            (nSfm1, x) = fn enumL n sfm1
            (nSfm2, y) = fn enumR n sfm2
        in [ Iff (Atom newID) (f (Atom nSfm1) (Atom nSfm2)) ]
            ++ x
            ++ y
    fn counter n fm = case fm of
        (Atom i) -> (i,[])
        _ -> (counter n, bicond' offset (counter n) fm)

-- tree enumerators
enumL :: Word -> Word
enumL = flip shift 1
enumR :: Word -> Word
enumR = flip setBit 0 . flip shift 1

-- ------------------------------------------------------------------------------------

dimacs :: [[Formula VarID]] -> DIMACS
dimacs = (fmap . fmap) fn
  where
    fn (Atom i)       = fromIntegral i
    fn (Not (Atom i)) = (negate. fromIntegral) i
    fn _ = error "dimacs: formula is not a literal"

-- TODO:
--  * is Formula a a monoid?
--  * is Formua a foldable?

