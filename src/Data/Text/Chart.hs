{-# LANGUAGE CPP #-}

module Data.Text.Chart
    ( plot
    , plotWith
    , options
    , height
    ) where

import Control.Monad (forM_)
import Data.Array.IO (newArray, IOArray, getElems, writeArray)
import Data.List     (unfoldr)
import Text.Printf   (printf)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

data Options =
  Options { height :: Int }

-- default options
options :: Options
options =
  Options { height = 14 }

newArray2D :: Integer -> Integer -> IO (IOArray (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

plot :: [Integer] -> IO ()
plot = plotWith options

pad :: Integral a => [a] -> Int
pad series =
  let floats = fromIntegral <$> series
      toStr :: [Float] -> [String]
      toStr = fmap (printf "%0.2f")
  in  maximum $ length <$> toStr floats

plotWith :: Options -> [Integer] -> IO ()
plotWith options series = do

    -- variables and functions
    let min' = minimum series
    let max' = maximum series
    let range = abs $ max' - min'
    let offset' = 3
    let height' = height options
    let ratio = fromIntegral height' / fromIntegral range :: Float
    let min2 = fromIntegral min' * ratio
    let max2 = fromIntegral max' * ratio
    let rows = round $ abs $ max2 - min2
    let width = length series + 3
    let pad' = pad series

    -- array creation
    arr <- newArray2D rows (toInteger width)
    let write arr [x] [y] = writeArray arr (x, y)
    let result = write arr

    -- axis and labels
    forM_ [min2..max2] $ \y -> do
            let label = fromInteger max' - (y - min2)
                      * fromInteger range / fromIntegral rows
            result [round(y - min2)] [maximum [offset' - 5, 0]] $
                   printf ("%"++ show pad' ++".2f") label
            result [round(y - min2)] [offset' - 1] $
                   if y == 0 then "┼" else "┤"

    -- initial value
    let first = fromInteger (head series) * ratio - min2
    result [round(fromInteger rows - first)] [offset' - 1] "┼"

    -- plot the line
    forM_ [0..(length series - 2)] $ \x' -> do
        let x = toInteger x'
        let offset'' = x + offset'
        let y0 = round (fromInteger (series !! (x' + 0)) * ratio) - round min2
        let y1 = round (fromInteger (series !! (x' + 1)) * ratio) - round min2
        if y0 == y1 then
            result [rows - y0] [offset''] "─"
        else do
            result [rows - y1] [offset''] $
                   if y0 > y1 then "╰" else "╭"
            result [rows - y0] [offset''] $
                   if y0 > y1 then "╮" else "╯"
            let start = minimum [y0, y1] + 1
            let end = maximum [y0, y1]

            forM_ [start..(end - 1)] $ \y ->
                result [rows - y] [offset''] "│"

    -- print the results
    elements <- getElems arr
    let result = splitEvery (width + 1) elements
    forM_ result $ putStrLn . concat
