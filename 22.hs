module TwentyTwo where
import Data.Monoid

range :: Integer -> Integer -> [Integer]
range x y = go [] x y
  where go r x' y'
          | x' > y' = r
          | x' == y' = (r <> [x'])
          | otherwise = go (r <> [x']) (x' + 1) y'
