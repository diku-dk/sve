{-# LANGUAGE DeriveGeneric #-}

-- | Successive variable elimination.
--
-- This can be used to perform boolean unification.  To solve the
-- unification problem @A = B@ where @A@ and @B@ are boolean formulas,
-- define @C = A xor B@ and solve the matching problem @C = false@
-- with 'sve'.  Each of the resulting substitutions (if any)
-- constitute a most general unifier (mgu).
module SVE
  ( sve,
    Formula (..),
    Op (..),
    fvs,
    Subst,
    subst,
  )
where

import Control.Applicative
import Control.DeepSeq
import Data.Foldable hiding (and, or)
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Traversable
import GHC.Generics (Generic, Generic1)
import Prelude hiding (and, or)

nubByOrd :: (a -> a -> Ordering) -> [a] -> [a]
nubByOrd cmp = map NE.head . NE.groupBy eq . sortBy cmp
  where
    eq x y = cmp x y == EQ

-- | Like @nub@, but without the quadratic runtime.
nubOrd :: Ord a => [a] -> [a]
nubOrd = nubByOrd compare

-- | A boolean operation.
data Op = And | Or | Xor
  deriving (Eq, Ord, Show, Generic)

instance NFData Op

-- | A boolean formula parameterised by the representation of
-- variables.
data Formula v
  = T
  | F
  | Var v
  | Not (Formula v)
  | Op Op (Formula v) (Formula v)
  deriving (Eq, Ord, Show, Generic, Generic1)

instance Functor Formula where
  fmap = fmapDefault

instance Foldable Formula where
  foldMap = foldMapDefault

instance Traversable Formula where
  traverse _ T = pure T
  traverse _ F = pure F
  traverse f (Var v) = Var <$> f v
  traverse f (Not x) = Not <$> traverse f x
  traverse f (Op o x y) = Op o <$> traverse f x <*> traverse f y

instance NFData a => NFData (Formula a)

instance NFData1 Formula

or, and :: Formula v -> Formula v -> Formula v
or = Op Or
and = Op And

-- | The free variables of a formula.  Free of duplicates.
fvs :: Ord v => Formula v -> [v]
fvs = nubOrd . toList

-- | A substitution is a mapping from a variable to a formula.
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

-- | Apply a substitution to a formula.
subst :: Ord v => Subst v -> Formula v -> Formula v
subst _ T = T
subst _ F = F
subst m (Not f) = simplify $ Not $ subst m f
subst m (Var v) = fromMaybe (Var v) $ M.lookup v m
subst m (Op op x y) = simplify (Op op (subst m x) (subst m y))

-- | Given a formula and a list of variables, produce possible
-- substitutions for the variables that would cause the formula to
-- become false.  Any variable not in the list will be considered a
-- constant.  Use 'fvs' if you want to solve for all variables.
sve :: (Eq v, Ord v, Show v) => Formula v -> [v] -> [Subst v]
sve orig vs = worker (nubOrd vs) orig
  where
    worker _ F = [mempty]
    worker [] _ = []
    worker (x : vs_without_x) f = do
      let t0 = subst (M.singleton x F) f
          t1 = subst (M.singleton x T) f
      s <- worker vs_without_x $ simplify $ Op And t0 t1
      let xf =
            simplify $
              subst s t0 `or` simplify (Var x `and` simplify (Not (subst s t1)))
      pure $ M.insert x xf s
