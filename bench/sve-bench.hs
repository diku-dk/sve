module Main (main) where

import Criterion.Main
import SVE
import Prelude hiding (and, or)

or, and, xor :: Formula v -> Formula v -> Formula v
or = Op Or
and = Op And
xor = Op Xor

formula :: Formula Int
formula = ((Var 0 `and` Var 1) `xor` (Var 2 `and` Var 3))

main :: IO ()
main =
  defaultMain
    [ bgroup
        "sve"
        [ bench "formula" $ nf sve formula
        ],
      bgroup
        "sve2"
        [ bench "formula" $ nf sve2 formula
        ]
    ]
