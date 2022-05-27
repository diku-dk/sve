module Main (main) where

import qualified Data.Map as M
import SVE
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (and, or)

or, and, xor :: Formula v -> Formula v -> Formula v
or = Op Or
and = Op And
xor = Op Xor

testSolve :: (Show v, Ord v) => Formula v -> Subst v -> TestTree
testSolve f expected =
  testCase (show f) $
    (expected `elem` sve f) @? "does not produce expected subst"

allTests :: TestTree
allTests =
  testGroup
    "sve"
    [ testSolve ((Var 0 `and` Var 1) `xor` (Var 2 `and` Var 3)) $
        M.fromList [(0, Op Or (Op And (Op And (Var 2) (Not (Op And (Var 3) (Not (Var 1))))) (Var 3)) (Op And (Var 0) (Not (Op Xor (Var 1) (Op And (Op And (Var 2) (Not (Op And (Var 3) (Not (Var 1))))) (Var 3)))))), (2, Op And (Var 2) (Not (Op And (Var 3) (Not (Var 1)))))]
    ]

main :: IO ()
main = defaultMain allTests
