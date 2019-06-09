{-# LANGUAGE TupleSections #-}

module HaskellWorks.Data.BalancedParens.Gen
  ( BP(..)
  , count
  , bpBools
  , showBps
  , storableVector
  , bpParensSeq
  , vector
  , vec2
  ) where

import Data.Coerce
import Data.Semigroup                                      ((<>))
import HaskellWorks.Data.BalancedParens.Internal.ParensSeq (ParensSeq)
import HaskellWorks.Data.Positioning
import Hedgehog

import qualified Data.Vector                                         as DV
import qualified Data.Vector.Storable                                as DVS
import qualified HaskellWorks.Data.BalancedParens.Internal.ParensSeq as PS
import qualified Hedgehog.Gen                                        as G

count :: MonadGen m => Range Count -> m Count
count r = coerce <$> G.word64 (coerce <$> r)

data LR a = L a Int | R a Int deriving (Eq, Show)

newtype BP = BP [Bool] deriving Eq

showBps :: [Bool] -> String
showBps = fmap fromBool
  where fromBool True  = '('
        fromBool False = ')'

bpBools' :: MonadGen m => Int -> (Int, [Bool], [Bool], Int) -> m [Bool]
bpBools' n (ln, lt, rt, rn) = if n <= 0
  then return (reverse lt <> rt)
  else if ln - rn >= n
    then return (reverse lt <> replicate n False <> rt)
    else if rn - ln >= n
      then return (reverse lt <> replicate n True <> rt)
      else do
        decision <- case (ln, rn) of
          (0, 0) -> G.element [L '(' 1,                         R ')' 1]
          (0, _) -> G.element [L '(' 1,             R '(' (-1), R ')' 1]
          (_, 0) -> G.element [L '(' 1, L ')' (-1),             R ')' 1]
          _      -> G.element [L '(' 1,                         R ')' 1]

        case decision of
          L p d -> bpBools' (n - 1) (ln + d, toBool p:lt,          rt, rn    )
          R p d -> bpBools' (n - 1) (ln    ,          lt, toBool p:rt, rn + d)
  where toBool '(' = True
        toBool  _  = False

bpBools ::  MonadGen m => Range Int -> m [Bool]
bpBools r = do
  n <- G.int r
  bpBools' (n * 2) (0, [], [], 0)

bpParensSeq ::  MonadGen m => Range Int -> m ParensSeq
bpParensSeq = fmap PS.fromBools . bpBools

storableVector :: (MonadGen m, DVS.Storable a) => Range Int -> m a -> m (DVS.Vector a)
storableVector r g = DVS.fromList <$> G.list r g

vector :: MonadGen m => Range Int -> m a -> m (DV.Vector a)
vector r g = DV.fromList <$> G.list r g

vec2 :: MonadGen m => m a -> m (a, a)
vec2 g = (,) <$> g <*> g
