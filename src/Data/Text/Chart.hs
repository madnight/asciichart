module Data.Text.Chart (plot) where

import Control.Monad (forM_)
import Data.Array.IO (newArray, IOArray, getElems, writeArray)
import Data.List     (unfoldr)
import Text.Printf   (printf)

data Options =
  Options { height :: Int
          , offset :: Int
          }

-- default options
options :: Options
options =
  Options { height = 14
          , offset = 3
          }

newArray2D :: Integer -> Integer -> IO (IOArray (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

plot :: [Integer] -> IO ()
plot = plotWith options

plotWith :: Options -> [Integer] -> IO ()
plotWith options series = do

    -- variables and functions
    let min' = minimum series
    let max' = maximum series
    let range = abs $ max' - min'
    let offset' = fromIntegral $ offset options
    let height' = height options
    let ratio = fromIntegral height' / fromIntegral range :: Float
    let min2 = fromIntegral min' * ratio
    let max2 = fromIntegral max' * ratio
    let rows = round $ abs $ max2 - min2
    let width = length series + 3

    -- array creation
    arr <- newArray2D rows (toInteger width)
    let write arr [x] [y] = writeArray arr (x, y)
    let result = write arr

    -- axis and labels
    forM_ [min2..max2] $ \y -> do
            let label = fromInteger max' - (y - min2)
                      * fromInteger range / fromIntegral rows
            result [round(y - min2)] [maximum [offset' - 5, 0]]
                   (printf "%6.2f" label)
            result [round(y - min2)] [offset' - 1]
                   (if y == 0 then "┼" else "┤")

    -- initial value
    let first = fromInteger (head series) * ratio - min2
    result [round(fromInteger rows - first)] [offset' - 1] "┼"

    -- plot the line
    forM_ [0..(length series - 2)] $ \x' -> do
        let x = toInteger x'
        let y0' = round (fromInteger (series !! (x' + 0)) * ratio) - round min2
        let y1' = round (fromInteger (series !! (x' + 1)) * ratio) - round min2
        if y0' == y1' then
            result [rows - y0'] [x + offset'] "─"
        else do
            result [rows - y1'] [x + offset'] $
                   if y0' > y1' then "╰" else "╭"
            result [rows - y0'] [x + offset'] $
                   if y0' > y1' then "╮" else "╯"
            let start = minimum [y0', y1'] + 1
            let end = maximum [y0', y1']

            forM_ [start..(end - 1)] $ \y ->
                result [rows - y] [toInteger x + offset'] "│"

    -- print the results
    elements <- getElems arr
    let result = splitEvery (width + 1) elements
    forM_ result $ putStrLn . concat
