module Nnaskell ( NN(..)
                , randomNN
                , activateNN
                , cost
                , optimizeCost
                , loadNN
                , saveNN
                ) where

import Data.Function
import Data.List
import Control.Monad
import System.Random
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix

data NN = NN { nnWs :: [Matrix R]
             , nnBs :: [Vector R]
             } deriving (Show, Read)

randomVec :: Int -> IO (Vector R)
randomVec n = vector <$> (replicateM n $ randomRIO (-1.0, 1.0))

randomMatrix :: Int -> Int -> IO (Matrix R)
randomMatrix n m = matrix m <$> (replicateM ((*) n m) $ randomRIO (-1.0, 1.0))

randomLayer :: Int -> Int -> IO (Matrix R, Vector R)
randomLayer n m = liftM2 (\ws bs -> (ws, bs)) (randomMatrix m n) (randomVec m)

randomNN :: [Int] -> IO NN
randomNN ns = do (ws, bs) <- unzip <$> (sequence $ map (uncurry randomLayer) $ pairScan ns)
                 return $ NN { nnWs = ws
                             , nnBs = bs
                             }

sigmoid :: R -> R
sigmoid t = 1 / (1 + exp (-1 * t))

sigmoidVec :: Vector R -> Vector R
sigmoidVec = fromList . map sigmoid . toList

activateLayer :: Vector R -> (Matrix R, Vector R) -> Vector R
activateLayer as (ws, bs) = sigmoidVec (ws #> as + bs)

activateNN :: NN -> Vector R -> Vector R
activateNN nn as = foldl activateLayer as $ zip ws bs
    where ws = nnWs nn
          bs = nnBs nn

pairScan :: [a] -> [(a, a)]
pairScan (x:y:rest) = (x, y) : pairScan (y:rest)
pairScan _ = []

cost :: [(Vector R, Vector R)] -> NN -> R
cost td nn = (sum $ map inputCost td) / n
    where inputCost (input, output) = sumElements $ cmap (** 2) (activateNN nn input - output)
          n = fromIntegral $ length td

stepBias :: NN -> Int -> R -> NN
stepBias nn updateIdx s = nn { nnBs = map (\(v, idx) -> if idx <= updateIdx && updateIdx < idx + size v
                                                        then accum v (+) [(updateIdx - idx, s)]
                                                        else v)
                                      $ zip bs
                                      $ scanl (\idx v -> idx + size v) 0 bs
                             }
    where bs = nnBs nn

stepWeight :: NN -> Int -> R -> NN
stepWeight nn updateIdx s = nn { nnWs = map (\(mx, idx) -> let (n, m) = size mx
                                                               effIdx = updateIdx - idx
                                                           in if idx <= updateIdx && updateIdx < idx + (n * m)
                                                              then accum mx (+) [((effIdx `div` m, effIdx `mod` m), s)]
                                                              else mx)
                                        $ zip ws
                                        $ scanl (\idx m -> idx + uncurry (*) (size m)) 0 ws
                               }
    where ws = nnWs nn

class Domain d where
    countArgs :: d -> Int
    stepArg :: d -> Int -> R -> d

instance Domain NN where
    countArgs nn = bsCount + wsCount
        where bsCount = sum $ map (length . toList) $ nnBs nn
              wsCount = sum $ map length $ concatMap toLists $ nnWs nn
    stepArg nn idx s = if idx < bsCount
                       then stepBias nn idx s
                       else stepWeight nn (idx - bsCount) s
        where bsCount = sum $ map (length . toList) $ nnBs nn

optimizeCost :: Domain d => (d -> R) -> d -> [d]
optimizeCost cost d = scanl optimizeStep d $ cycle [0 .. n - 1]
    where n = countArgs d
          optimizeStep d idx = minimumBy (compare `on` cost) $ map (stepArg d idx) [-step, 0, step]
          step = 0.1

loadNN :: FilePath -> IO NN
loadNN filePath = read <$> readFile filePath

saveNN :: FilePath -> NN -> IO ()
saveNN filePath nn = writeFile filePath $ show nn
