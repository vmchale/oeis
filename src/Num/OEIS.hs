module Num.OEIS
    ( -- * Helper functions
      asBTextFile
    -- * Specific sequences
    , oeis000127
    ) where

factorial :: Int -> Integer
factorial n = product [1..(fromIntegral n)]

choose :: Int -> Int -> Integer
choose n k = product (fmap fromIntegral [(n-k+1)..n]) `div` factorial k

maxRegions :: Int -> Integer
maxRegions n = sum $ fmap (n `choose`) [0..4]

oeis000127 :: [Integer]
oeis000127 = fmap maxRegions [0..]

index :: [a] -> [(Int, a)]
index = zip [1..]

asBTextFile :: (Show a) => [a] -> String
asBTextFile = unlines . fmap showPair . index
    where showPair (i, j) = show i ++ " " ++ show j
