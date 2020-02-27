{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module HaskellWorks.Data.BalancedParens.Gen
  ( BP(..)
  , count
  , bpBools
  , showBps
  , storableVector
  , storableVectorWord64
  , bpParensSeq
  , vector
  , vec2
  , randomRm
  , randomRm2
  ) where

import Data.Coerce
import Data.Word
import GHC.Generics
import HaskellWorks.Data.BalancedParens.ParensSeq (ParensSeq)
import HaskellWorks.Data.Positioning
import Hedgehog
import Hedgehog.Internal.Gen
import Hedgehog.Internal.Seed

import qualified Data.Vector                                as DV
import qualified Data.Vector.Storable                       as DVS
import qualified HaskellWorks.Data.BalancedParens.ParensSeq as PS
import qualified HaskellWorks.Data.BalancedParens.RangeMin  as RM
import qualified HaskellWorks.Data.BalancedParens.RangeMin2 as RM2
import qualified Hedgehog.Gen                               as G
import qualified Hedgehog.Range                             as R

count :: MonadGen m => Range Count -> m Count
count r = coerce <$> G.word64 (coerce <$> r)

data LR a = L a Int | R a Int deriving (Eq, Show)

newtype BP = BP [Bool] deriving (Eq, Generic)

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

randomRm :: MonadGen m => Range Int -> m (RM.RangeMin (DVS.Vector Word64))
randomRm r = do
  v <- storableVector (fmap (64 *) r) (G.word64 R.constantBounded)
  return (RM.mkRangeMin v)

randomRm2 :: MonadGen m => Range Int -> m (RM2.RangeMin2 (DVS.Vector Word64))
randomRm2 r = do
  v <- storableVector (fmap (64 *) r) (G.word64 R.constantBounded)
  return (RM2.mkRangeMin2 v)

withGenT :: (MonadGen m, MonadGen n) => (GenT (GenBase m) a -> GenT (GenBase n) b) -> m a -> n b
withGenT f = fromGenT . f . toGenT

storableVectorWord64 :: MonadGen m => Range Int -> m Word64 -> m (DVS.Vector Word64)
storableVectorWord64 r = withGenT $ \g -> do
  n <- integral_ r
  GenT $ \_ seed ->
    return $ flip (DVS.unfoldrN n) seed $ \s ->
      case nextWord64 s of
        (w, sb) -> Just (w, sb)
