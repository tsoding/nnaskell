module MNIST ( readLabelsFile
             , readImagesFile
             ) where

import Text.Printf
import Data.Word
import qualified Data.ByteString.Lazy as BL

data Image = Image { imageSize :: (Int, Int)
                   , imageData :: [Word8]
                   } deriving (Show)

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
       then return
            $ map (\bs -> Image { imageSize = (rows, cols)
                                , imageData = bs
                                })
            $ chunks (rows * cols)
            $ BL.unpack body
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

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

pixelToChar :: Word8 -> Char
pixelToChar p = fst $ head $ dropWhile (\(_, i) -> p > i) pallete
    where pallete = [ ('░', (255 `div` n))
                    , ('▒', (255 `div` n) * 2)
                    , ('▓', (255 `div` n) * 3)
                    , ('█', 255) ]
          n = 4

displayImage :: Image -> String
displayImage image = unlines
                     $ chunks (2 * cols)
                     $ concatMap (replicate 2 . pixelToChar)
                     $ imageData image
    where (_, cols) = imageSize image
