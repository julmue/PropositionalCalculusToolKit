import Data.Formula

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Control.Monad (liftM, liftM2)
-- import Text.Show.Functions

-- main = defaultMain tests
main = defaultMain $ formulaFunctorLaws


-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- QuickCheck Tests

formulaFunctorLaws = testGroup "Formula Functor Laws"
    [ QC.testProperty "Formula Functor Law 1"
        ((\fm -> fmap id fm == id fm ) :: Formula Int -> Bool)
    , QC.testProperty "Formula Functor Law 2"
        ((\ p q fm -> fmap (p . q) fm == fmap p (fmap q fm)) :: (Int -> Int) -> (Int -> Int) -> Formula Int -> Bool)
    ]

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- QuickCheck Test Preparation

-- create a new wrapper for variable values for better control
-- of the distrubution intervall
newtype TInt = TInt  Int deriving (Eq, Read, Show)

-- make id type and formula instance of class arbitrary for data generation
instance Arbitrary TInt where
    arbitrary = sized tint'
      where
        tint' n = do
            x <- choose (0,n)
            return . TInt $ x

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
-- sample ((arbitrary) :: Gen (Formula Int))

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
