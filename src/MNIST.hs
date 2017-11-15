module MNIST ( readLabelsFile
             , readImagesFile
             ) where

import Text.Printf
import qualified Data.ByteString.Lazy as BL

type Image = [Int]

bytesAsInt :: BL.ByteString -> Int
bytesAsInt = foldl (\a x -> a * 0x100 + x) 0 . map fromIntegral . BL.unpack

parseInt32 :: BL.ByteString -> (Int, BL.ByteString)
parseInt32 bs = (bytesAsInt $ BL.take 4 bs, BL.drop 4 bs)

magicNumberParser :: Int -> BL.ByteString -> IO BL.ByteString
magicNumberParser expectedNumber bs
    | expectedNumber == actualNumber = return rest
    | otherwise = ioError
                  $ userError
                  $ printf "Unexpected magic number: %d. Expected %d." actualNumber expectedNumber
    where (actualNumber, rest) = parseInt32 bs

labelsParser :: BL.ByteString -> IO [Int]
labelsParser bs =
    let (expectedCount, body) = parseInt32 bs
        labels = map fromIntegral $ BL.unpack body
        actualCount = length labels
    in if expectedCount == actualCount
       then return labels
       else ioError
            $ userError
            $ printf "Unexpected amount of labels. Expected: %d, but got %d" expectedCount actualCount

readLabelsFile :: FilePath -> IO [Int]
readLabelsFile fileName =
    BL.readFile fileName
    >>= magicNumberParser 2049
    >>= labelsParser

readImagesFile :: FilePath -> IO [Image]
readImagesFile = undefined
