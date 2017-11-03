module Nnaskell where

import Control.Monad
import System.Random
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix

type Layer = (Matrix R, Vector R)
type NN = [Layer]

randomVec :: Int -> IO (Vector R)
randomVec n = vector <$> (replicateM n $ randomRIO (-1.0, 1.0))

randomMatrix :: Int -> Int -> IO (Matrix R)
randomMatrix n m = matrix m <$> (replicateM ((*) n m) $ randomRIO (-1.0, 1.0))

randomLayer :: Int -> Int -> IO Layer
randomLayer n m = liftM2 (\ws bs -> (ws, bs)) (randomMatrix m n) (randomVec m)

randomNN :: [Int] -> IO NN
randomNN ns = sequence $ map (uncurry randomLayer) $ pairScan ns

sigmoid :: R -> R
sigmoid t = 1 / (1 + exp (-1 * t))

sigmoidVec :: Vector R -> Vector R
sigmoidVec = fromList . map sigmoid . toList

activateLayer :: Vector R -> Layer -> Vector R
activateLayer as (ws, bs) = sigmoidVec (ws #> as + bs)

classify :: NN -> Vector R -> Vector R
classify nn as = foldl activateLayer as nn

pairScan :: [a] -> [(a, a)]
pairScan (x:y:rest) = (x, y) : pairScan (y:rest)
pairScan _ = []
