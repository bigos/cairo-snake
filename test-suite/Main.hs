import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Snake (shrink)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [ qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "shrink above zero" $
    \x -> (x) >= 1 QC.==> Snake.shrink x == (x - 1)
  , QC.testProperty "shrink on or below zero" $
    \x -> (x) <= 0 QC.==> Snake.shrink x == 0
  , QC.testProperty "my addition" $ -- my first quickCheck
    \x -> (((x :: Integer) + 1) > x) == True
  ]
