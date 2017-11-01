module Nnaskell where

import Control.Monad
import System.Random
import qualified Data.Matrix as M
import qualified Data.Vector as V

type Layer = (M.Matrix Float, V.Vector Float)
type NN = [Layer]

randomVector :: Int -> IO (V.Vector Float)
randomVector n = V.fromList <$> (replicateM n $ randomRIO (-1.0, 1.0))

randomMatrix :: Int -> Int -> IO (M.Matrix Float)
randomMatrix n m = M.fromList n m <$> (replicateM ((*) n m) $ randomRIO (-1.0, 1.0))

randomLayer :: Int -> Int -> IO Layer
randomLayer n m = liftM2 (\ws bs -> (ws, bs)) (randomMatrix m n) (randomVector m)

randomNN :: [Int] -> IO NN
randomNN ns = sequence $ map (uncurry randomLayer) $ pairScan ns

sigmoid :: Float -> Float
sigmoid t = 1 / (1 + exp (-1 * t))

-- NN persistance

classify :: NN -> V.Vector Float -> V.Vector Float
classify nn as = M.getCol 1 $ foldl (\as (ws, bs) -> M.mapCol (const sigmoid) 1 (ws * as + M.colVector bs)) (M.colVector as) nn

-- randomNN [4, 4, 4, 2] -> [[4, 4], [4, 4], [4, 2]]

pairScan :: [a] -> [(a, a)]
pairScan (x:y:rest) = (x, y) : pairScan (y:rest)
pairScan _ = []
