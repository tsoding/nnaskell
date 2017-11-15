module MNIST ( readLabelsFile
             , readImagesFile
             ) where

import Text.Printf
import Data.Word
import qualified Data.ByteString.Lazy as BL

data Image = Image { imageSize :: (Int, Int)
                   , imageData :: [Word8]
                   }

bytesAsInt :: BL.ByteString -> Int
bytesAsInt = foldl (\a x -> a * 0x100 + x) 0 . map fromIntegral . BL.unpack

magicNumberParser :: Int -> BL.ByteString -> IO BL.ByteString
magicNumberParser expectedNumber bs
    | expectedNumber == actualNumber = return rest
    | otherwise = ioError
                  $ userError
                  $ printf "Unexpected magic number: %d. Expected %d." actualNumber expectedNumber
    where actualNumber = bytesAsInt $ BL.take 4 bs
          rest = BL.drop 4 bs

labelsParser :: BL.ByteString -> IO [Word8]
labelsParser bs =
    let expectedCount = bytesAsInt $ BL.take 4 bs
        body = BL.drop 4 bs
        labels = BL.unpack body
        actualCount = length labels
    in if expectedCount == actualCount
       then return labels
       else ioError
            $ userError
            $ printf "Unexpected amount of labels. Expected: %d, but got %d" expectedCount actualCount

imagesParser :: BL.ByteString -> IO [Image]
imagesParser bs =
    let expectedCount = bytesAsInt $ BL.take 4 bs
        rows = bytesAsInt $ BL.take 4 $ BL.drop 4 bs
        cols = bytesAsInt $ BL.take 4 $ BL.drop 8 bs
        body = BL.drop 12 bs
        actualCount = fromIntegral $ BL.length body
    in if expectedCount * rows * cols == actualCount
       then undefined
       else ioError
            $ userError
            $ printf "Unexpected amount of bytes. Expected: %d, but got %d" (expectedCount * rows * cols) actualCount

readLabelsFile :: FilePath -> IO [Word8]
readLabelsFile fileName =
    BL.readFile fileName
    >>= magicNumberParser 2049
    >>= labelsParser

readImagesFile :: FilePath -> IO [Image]
readImagesFile fileName =
    BL.readFile fileName
    >>= magicNumberParser 2051
    >>= imagesParser
