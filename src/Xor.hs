module Xor where

import Nnaskell
import Numeric.LinearAlgebra.Data

xorNN = do nn <- randomNN [2, 2, 1]
            return $ head $ drop 5000 $ optimizeCost (cost xorTD) nn

xorTD :: [(Vector R, Vector R)]
xorTD = [ (vector [0.0, 0.0], vector [0.0])
        , (vector [1.0, 0.0], vector [1.0])
        , (vector [0.0, 1.0], vector [1.0])
        , (vector [1.0, 1.0], vector [0.0])
        ]
