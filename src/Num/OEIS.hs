module Num.OEIS
    ( write000127
    ) where

import           Numeric.Combinatorics (choose)

maxRegions :: Int -> Integer
maxRegions n = sum $ fmap (n `choose`) [0..4]

write000127 :: IO ()
write000127 = writeFile "b000127.txt" (asBTextFile (maxRegions <$> [0..45000]))

index :: [a] -> [(Int, a)]
index = zip [1..]

asBTextFile :: (Show a) => [a] -> String
asBTextFile = unlines . fmap showPair . index
    where showPair (i, j) = show i ++ " " ++ show j
