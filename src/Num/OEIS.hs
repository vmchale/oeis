module Num.OEIS
    ( -- * Helper functions
      asBTextFile
    -- * Specific sequences
    , oeis000127
    -- * Data generation
    , write000127
    ) where

import           Numeric.Combinatorics (choose)

maxRegions :: Int -> Integer
maxRegions n = sum $ fmap (n `choose`) [0..4]

oeis000127 :: [Integer]
oeis000127 = fmap maxRegions [0..45000]

write000127 :: IO ()
write000127 = writeFile "b000127.txt" (asBTextFile oeis000127)

index :: [a] -> [(Int, a)]
index = zip [1..]

asBTextFile :: (Show a) => [a] -> String
asBTextFile = unlines . fmap showPair . index
    where showPair (i, j) = show i ++ " " ++ show j
