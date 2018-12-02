module Solutions.Day02 where

import Prelude hiding (null)
import Data.Set (empty, fromList, null, member, insert, delete)

day02a :: [String] -> Int
day02a ids = n * m
  where
    (n, m) = foldl outer (0, 0) ((foldl inner initMem) <$> ids)

    outer (n, m) (_, _, s2, s3) = (n + nInc, m + mInc)
      where
        nInc = fromEnum $ not $ null s2
        mInc = fromEnum $ not $ null s3

    inner (s0, s1, s2, s3) x | member x s0 = (delete x s0, insert x s1, s2, s3)
    inner (s0, s1, s2, s3) x | member x s1 = (s0, delete x s1, insert x s2, s3)
    inner (s0, s1, s2, s3) x | member x s2 = (s0, s1, delete x s2, insert x s3)
    inner (s0, s1, s2, s3) x | member x s3 = (s0, s1, s2, delete x s3)
    inner sets             _               = sets

    initMem = (fromList ['a'..'z'], empty, empty, empty)
