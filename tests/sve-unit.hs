module Main (main) where

import qualified Data.Map as M
import SVE
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (and, or)

and, xor :: Formula v -> Formula v -> Formula v
and = Op And
xor = Op Xor

testSolve :: Formula Int -> Subst Int -> TestTree
testSolve f expected =
  testCase (show f) $
    (expected `elem` sve' f) @? "does not produce expected subst"
  where
    sve' x = sve x (fvs x)

allTests :: TestTree
allTests =
  testGroup
    "sve"
    [ testSolve ((Var 0 `and` Var 1) `xor` (Var 2 `and` Var 3)) $
        M.fromList [(0, Op Or (Op And (Var 2) (Var 3)) (Op And (Var 0) (Not (Op Xor (Op Or (Op And (Var 2) (Var 3)) (Var 1)) (Op And (Var 2) (Var 3)))))), (1, Op Or (Op And (Var 2) (Var 3)) (Var 1))]
    ]

main :: IO ()
main = defaultMain allTests
