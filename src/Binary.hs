module Binary where

import Nnaskell
import Numeric.LinearAlgebra.Data

binaryNN :: IO NN
binaryNN = loadNN "./data/binary.txt"

binaryTD :: [(Vector R, Vector R)]
binaryTD = zip (map binaryInput ns) (map binaryOutput ns)
    where ns = [0 .. 15]

binaryInput :: Int -> Vector R
binaryInput x = vector $ map fromIntegral [ x `mod` 2
                                          , (x `div` 2) `mod` 2
                                          , (x `div` 4) `mod` 2
                                          , (x `div` 8) `mod` 2]

binaryOutput :: Int -> Vector R
binaryOutput x = accum (vector $ replicate 16 0.0) (\a _ -> a) [(x, 1.0)]
