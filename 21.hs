module TwentyOne where
import Data.Monoid

insertAt :: a -> [a] -> Integer -> Maybe [a]
insertAt x xs i = go xs [] 1
  where go (t:ts) bs c =
               if c == i 
               then Just $ bs <> [x] <> (t:ts) 
               else go ts (t:bs) (c + 1)
        go [] bs c = 
               if c == i
               then Just $ bs <> [x] 
               else Nothing
  
