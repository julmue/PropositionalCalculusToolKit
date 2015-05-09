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

-- ----------------------------------------------------------------------------
main = defaultMain $ testGroup "tests"
    [ formulaFunctorLaws
    , atomsTests
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

domainTests = testGroup "domain test"
    [ testCase "domain1" $  2 @=? length (domain (Atom 1))
    , testCase "domain2" $  2 @=? length (domain (And (Atom 1) (Atom 1)))
    , testCase "domain2" $  4 @=? length (domain (And (Atom 1) (Atom 2)))
    ]

modelsTests = testGroup "models test"
    [ testCase "modelsAtom1" $  1 @=? length (models (Atom 1))
--    , testCase "modelsAtom2" $  [[(1,True)]] @=? (fmap fromList (models (Atom 1)))
    ]



cnfTests = testGroup "cnf test"
    [ QC.testProperty "semantic equality"
        ((\fm -> domain fm == domain (cnf fm)) :: (Formula VarID) -> Bool)
    ]


-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- QuickCheck Test Preparation

-- create a new wrapper for variable values for better control
-- of the distrubution intervall
-- newtype TInt = TInt  Int deriving (Eq, Read, Show)

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

instance (Monad m) => Serial m Word where
  series =
    generate (\d -> if d >= 0 then pure 0 else empty)
    where
      nats = generate $ \d -> [1..d]

-- to get the impression of the produced data:
-- sample ((arbitrary) :: Gen (Formula Int))

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
