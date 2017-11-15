module MNIST ( readLabelsFile
             , readImagesFile
             ) where

import Text.Printf
import qualified Data.ByteString.Lazy as BL

type Image = [Int]

list2Number :: BL.ByteString -> Int
list2Number = foldl (\a x -> a * 0xff + x) 0 . map fromIntegral . BL.unpack

labelsMagicNumberParser :: BL.ByteString -> IO BL.ByteString
labelsMagicNumberParser bs = let magicNumber = list2Number $ BL.take 4 bs
                                 body = BL.drop 4 bs
                             in if magicNumber == 2049
                                then return body
                                else ioError $ userError $ printf "Unexpected magic number: %d" magicNumber

labelsDataParser :: BL.ByteString -> IO [Int]
labelsDataParser bs = undefined

readLabelsFile :: FilePath -> IO [Int]
readLabelsFile fileName =
    BL.readFile fileName >>= labelsMagicNumberParser >>= labelsDataParser

readImagesFile :: FilePath -> IO [Image]
readImagesFile = undefined
