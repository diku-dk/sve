{-# LANGUAGE DeriveGeneric #-}

-- | Successive variable elimination.
module SVE (sve, sve2, Op (..), Formula (..), Subst) where

import Control.DeepSeq
import Data.List (inits, sortBy, tails)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics (Generic, Generic1)
import Prelude hiding (and, or)

nubByOrd :: (a -> a -> Ordering) -> [a] -> [a]
nubByOrd cmp = map NE.head . NE.groupBy eq . sortBy cmp
  where
    eq x y = cmp x y == EQ

-- | Like @nub@, but without the quadratic runtime.
nubOrd :: Ord a => [a] -> [a]
nubOrd = nubByOrd compare

data Op = And | Or | Xor
  deriving (Eq, Ord, Show, Generic)

instance NFData Op

data Formula v
  = T
  | F
  | Var v
  | Not (Formula v)
  | Op Op (Formula v) (Formula v)
  deriving (Eq, Ord, Show, Generic, Generic1)

instance NFData a => NFData (Formula a)

instance NFData1 Formula

or, and, xor :: Formula v -> Formula v -> Formula v
or = Op Or
and = Op And
xor = Op Xor

fvs :: Formula v -> [v]
fvs T = []
fvs F = []
fvs (Var v) = [v]
fvs (Op _ x y) = fvs x ++ fvs y
fvs (Not f) = fvs f

type Subst v = M.Map v (Formula v)

simplify :: Eq v => Formula v -> Formula v
simplify (Op Or T _) = T
simplify (Op Or _ T) = T
simplify (Op Or F f) = f
simplify (Op Or f F) = f
simplify (Op And _ F) = F
simplify (Op And F _) = F
simplify (Op And f T) = f
simplify (Op And T f) = f
simplify (Op And a (Op Xor b c))
  | a == b = simplify (Op And a (Not c))
  | a == c = simplify (Op And a (Not b))
simplify (Op Xor T f) = Not f
simplify (Op Xor f T) = Not f
simplify (Op Xor f F) = f
simplify (Op Xor F f) = f
simplify (Not T) = F
simplify (Not F) = T
simplify x = x

subst :: Ord v => Subst v -> Formula v -> Formula v
subst _ T = T
subst _ F = F
subst m (Not f) = simplify $ Not $ subst m f
subst m (Var v) = fromMaybe (Var v) $ M.lookup v m
subst m (Op op x y) = simplify (Op op (subst m x) (subst m y))

sve :: (Eq v, Ord v, Show v) => Formula v -> [Subst v]
sve F = [mempty]
sve f = do
  x <- fvs f
  let t0 = subst (M.singleton x F) f
      t1 = subst (M.singleton x T) f
  s <- sve (simplify (Op And t0 t1))
  pure $ M.insert x (simplify (subst s t0 `or` simplify (Var x `and` simplify (Not (subst s t1))))) s

sve2 :: (Eq v, Ord v, Show v) => Formula v -> [Subst v]
sve2 orig = worker (nubOrd $ fvs orig) orig
  where
    worker _ F = [mempty]
    worker [] _ = []
    worker (x : vs_without_x) f = do
      let t0 = subst (M.singleton x F) f
          t1 = subst (M.singleton x T) f
      s <- worker vs_without_x (simplify (Op And t0 t1))
      pure $ M.insert x (simplify (subst s t0 `or` simplify (Var x `and` simplify (Not (subst s t1))))) s

appendix1 :: Formula String
appendix1 = ((Var "a" `and` Var "b") `xor` (Var "c" `and` Var "d"))
