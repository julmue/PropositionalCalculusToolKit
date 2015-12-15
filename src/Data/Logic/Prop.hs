module Data.Logic.Prop
where

import Prelude hiding (lookup)

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
onAtoms f fm = case fm of
    Atom x -> Atom (f x)
    Not p -> Not $ oa p
    And p q -> And (oa p) (oa q)
    Or  p q -> Or  (oa p) (oa q)
    Imp p q -> Imp (oa p) (oa q)
    Iff p q -> Iff (oa p) (oa q)
  where
    oa = onAtoms f

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
tseitin :: Formula VarID -> Formula VarID
tseitin a@(Atom _) = a
tseitin fm = allAnd . fmap cnf . bicond $ fm

bicond :: Formula Word -> [Formula Word]
bicond fm = (Atom rootID) : bicond' rootID fm
    where rootID = (succ . maximum . atoms) fm

bicond' :: Word -> Formula Word -> [Formula Word]
bicond' nodeID fm = case fm of
    Atom _ -> []
    Not (Atom _) -> [Iff (Atom nodeID) fm]
    Not sfm -> let newID = enumL nodeID
               in  [Iff (Atom nodeID) (Not (Atom newID))] ++ bicond' newID sfm
    And sfm1 sfm2 -> binFunc nodeID (And,sfm1,sfm2)
    Or  sfm1 sfm2 -> binFunc nodeID (Or ,sfm1,sfm2)
    Imp sfm1 sfm2 -> binFunc nodeID (Imp,sfm1,sfm2)
    Iff sfm1 sfm2 -> binFunc nodeID (Iff,sfm1,sfm2)
  where
    binFunc nodeID' (func, sfm1, sfm2) =
       let (nodeIDL, bcsL) = fn enumL nodeID' sfm1
           (nodeIDR, bcsR) = fn enumR nodeID' sfm2
       in [ Iff (Atom nodeID') (func (Atom nodeIDL) (Atom nodeIDR))] ++ bcsL ++ bcsR
    fn enumerator nodeID' f = case f of
        Atom i -> (i,[])
        _ -> (enumerator nodeID', bicond' (enumerator nodeID') fm)
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

