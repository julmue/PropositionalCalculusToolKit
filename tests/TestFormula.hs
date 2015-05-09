{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Data.Formula

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

-- import necessary to generate test values for own types
import Test.SmallCheck.Series

import Control.Monad (liftM, liftM2)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Applicative (empty)

import Data.Word (Word)
import Data.List (sort)
import Data.Map (Map, fromList, toList)

-- ----------------------------------------------------------------------------
main = defaultMain $ testGroup "tests"
    [ formulaFunctorLaws
    , atomsTests
    , evalTests
    , domainTests
    , modelsTests
    , cnfTests
    ]


-- ----------------------------------------------------------------------------
formulaFunctorLaws = testGroup "Formula Functor laws tests"
     [ QC.testProperty "Formula Functor Law 1"
        ((\fm -> fmap id fm == id fm ) :: Formula VarID -> Bool)
    , QC.testProperty "Formula Functor Law 2"
        ((\ p q fm -> fmap (p . q) fm == fmap p (fmap q fm)) :: (VarID -> VarID) -> (VarID -> VarID) -> Formula VarID -> Bool)
    ]


-- ----------------------------------------------------------------------------
atomsTests = testGroup "atoms tests"
    [ testCase "atomsAtom1" $   [1]   @=? atoms (Atom 1)
    , testCase "atomsNot1" $    [1]   @=? atoms (Not (Atom 1))
    , testCase "atomsAnd1" $    [1,2] @=? atoms (And (Atom 1) (Atom 2))
    , testCase "atomsAnd2" $    [1]   @=? atoms (And (Atom 1) (Atom 1))
    , testCase "atomsOr1" $     [1,2] @=? atoms (Or (Atom 1) (Atom 2))
    , testCase "AtomsOr2" $     [1]   @=? atoms (Or (Atom 1) (Atom 1))
    , testCase "atomsImp1" $    [1,2] @=? atoms (Imp (Atom 1) (Atom 2))
    , testCase "AtomsImp2" $    [1]   @=? atoms (Imp (Atom 1) (Atom 1))
    , testCase "atomsIff1" $    [1,2] @=? atoms (Iff (Atom 1) (Atom 2))
    , testCase "AtomsIff2" $    [1]   @=? atoms (Iff (Atom 1) (Atom 1))
    ]


-- ----------------------------------------------------------------------------
evalTests = testGroup "eval tests"
    [ evalAtomTests
    , evalNotTests
    ]

-- ----------------------------------------------------------------------------
evalAtomTests = testGroup "eval Atom tests"
    [ testCase "evalAtoms1" $ Just True     @=? eval (m [(1,True)]) (Atom 1)
    , testCase "evalAtoms1" $ Just False    @=? eval (m [(1,False)]) (Atom 1)
    ]
  where m = fromList

-- ----------------------------------------------------------------------------
evalNotTests = testGroup "eval Not tests"
    [ testCase "evalNot1" $ Just False     @=? eval (m [(1,True)]) (Not (Atom 1))
    , testCase "evalNot1" $ Just True    @=? eval (m [(1,False)]) (Not (Atom 1))
    ]
  where m = fromList

-- ----------------------------------------------------------------------------
domainTests = testGroup "domain test"
    [ testCase "domain1" $  2 @=? length (domain (Atom 1))
    , testCase "domain2" $  2 @=? length (domain (And (Atom 1) (Atom 1)))
    , testCase "domain2" $  4 @=? length (domain (And (Atom 1) (Atom 2)))
    ]

-- ----------------------------------------------------------------------------
modelsTests = testGroup "models test"
    [ testCase "modelsAtom1" $  1 @=? length (models (Atom 1))
    , testCase "modelsAtom2" $ [[(1,True)]] @=? (fmap toList . models $ Atom 1)
    ]

-- ----------------------------------------------------------------------------
cnfTests =
    localOption (SmallCheckDepth 3) $
    localOption (QuickCheckTests 100) $
    localOption (QuickCheckMaxSize 7) $
    testGroup "cnf test"
    [ SC.testProperty "semantic equality"
        ((\fm -> (sort . models) fm == (sort . models) (cnf fm)) :: (Formula VarID) -> Bool)
    , SC.testProperty "equal set of atoms"
        ((\fm -> (sort . atoms) fm == (sort . atoms) (cnf fm)) :: (Formula VarID) -> Bool)
    , QC.testProperty "semantic equality"
        ((\fm -> (sort . models) fm == (sort . models) (cnf fm)) :: (Formula VarID) -> Bool)
    , QC.testProperty "equal set of atoms"
        ((\fm -> (sort . atoms) fm == (sort . atoms) (cnf fm)) :: (Formula VarID) -> Bool)
    ]

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- QuickCheck Test Preparation
instance Arbitrary a => Arbitrary (Formula a) where
    arbitrary = sized formula'
      where
        formula' 0 = fmap Atom arbitrary
        formula' n | n>0 =
            let subf = subformula n
            in  oneof [ formula' 0
                      , liftM Not  subf
                      , liftM2 And subf subf
                      , liftM2 Or  subf subf
                      , liftM2 Imp subf subf
                      , liftM2 Iff subf subf
                      ]
        subformula n = formula' (n `div` 2)

-- to get the impression of the produced data:
-- sample ((arbitrary) :: Gen (Formula VarID))

-- Smallcheck Test Preparation
instance (Monad m) => Serial m Word where
  series =
    generate (\d -> if d >= 0 then pure (fromIntegral d) else empty)
    where
      nats = generate $ \d -> [1..]

-- to check the generated values: list x series :: [Formula VarID] (where x is some concrete Int -- the depth parameter)
instance Serial m a => Serial m (Formula a) where
  series =  cons1 Atom \/ cons1 Not \/ cons2 And \/ cons2 Or \/ cons2 Imp \/ cons2 Iff

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
