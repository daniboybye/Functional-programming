module SpamFilter.Algorithm
( uniq
, fastPow
) where

import Data.List

-- O(N*log N)
uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

-- O(log N)
fastPow :: (Num a, Integral b) => a -> b -> a
fastPow = h 1
    where h :: (Num a, Integral b) => a -> a -> b -> a
          h res _ 0 = res
          h res x n | mod n 2 == 1 = h (res * x) x $ n - 1
          h res x n = h res (x * x) $ div n 2 