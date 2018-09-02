module Num.OEIS
    ( -- * Print a sequence for upload
      asBTextFile
    -- * Specific sequences
    , oeis000002
    , oeis000004
    , oeis000007
    , oeis000012
    , oeis000027
    , oeis000030
    , oeis000032
    , oeis000034
    , oeis000035
    , oeis000038
    , oeis000042
    , oeis000127
    ) where

oeis000007 :: Integral a => [a]
oeis000007 = 1 : repeat 0

oeis000038 :: Integral a => [a]
oeis000038 = 2 : repeat 0

oeis000012 :: Integral a => [a]
oeis000012 = repeat 1

factorial :: Int -> Integer
factorial n = product [1..(fromIntegral n)]

choose :: Int -> Int -> Integer
choose n k = product (fmap fromIntegral [(n-k+1)..n]) `div` factorial k

maxRegions :: Int -> Integer
maxRegions n = sum $ fmap (n `choose`) [0..4]

-- | Due to John Tromp
oeis000002 :: Integral a => [a]
oeis000002 = 1 : 2 : (drop 2 . concat . zipWith replicate oeis000002 . cycle) [1, 2]

oeis000004 :: Integral a => [a]
oeis000004 = repeat 0

oeis000027 :: Integral a => [a]
oeis000027 = [1..]

oeis000032 :: Integral a => [a]
oeis000032 = 2 : 1 : zipWith (+) oeis000032 (tail oeis000032)

oeis000034 :: Integral a => [a]
oeis000034 = cycle [1,2]

oeis000035 :: Integral a => [a]
oeis000035 = cycle [0,1]

-- | Due to Reinhard Zumkeller
oeis000030 :: Integral a => [a]
oeis000030 = fmap initialDigit [1..]
    where initialDigit = until (< 10) (`quot` 10)

oeis000042 :: Integral a => [a]
oeis000042 = fmap unary [(1::Word)..]
    where unary = (`quot` 9) . subtract 1 . (10 ^)

oeis000127 :: [Integer]
oeis000127 = fmap maxRegions [0..]

index :: [a] -> [(Int, a)]
index = zip [1..]

asBTextFile :: (Show a) => [a] -> String
asBTextFile = unlines . fmap showPair . index
    where showPair (i, j) = show i ++ " " ++ show j
