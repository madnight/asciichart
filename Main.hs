{-# Language FlexibleContexts #-}

import Control.Lens
import Control.Monad.ST
import Control.Monad (forM_)
import Data.Array.IO
import Data.List.Split
import System.Environment
import Data.List
import Data.Char
import Data.Maybe

newArray2D :: Integer -> Integer -> IO (IOArray (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "


chart :: [Integer] -> IO()
chart series = do
    let min' = minimum series
    let max' = maximum series
    let range = abs $ max' - min'
    let offset = 3
    let ratio = range `div` range
    let min2 = min' * ratio
    let max2 = max' * ratio
    let rows = abs $ max2 - min2
    let width = length series + 3
    arr <- newArray2D rows (toInteger width)
    let write arr [x] [y] = writeArray arr (x, y)
    let result = write arr

    forM_ [min2..max2] $ \y -> do
            let label = show (max' - (y - min2) * range `div` rows)
            result [y - min2] [maximum [offset - 5, 0]] (padL 2 label)
            result [y - min2] [offset - 1] (if y == 0 then "┼" else "┤")

    let first = head series * ratio - min2
    result [rows - first] [offset - 1] "┼"

    forM_ [0..(length series - 2)] $ \x -> do
        let y0' = (series !! (x + 0) * ratio) - min2
        let y1' = (series !! (x + 1) * ratio) - min2

        if y0' == y1' then
            result [rows - y0'] [toInteger x + toInteger offset] "─"
        else do
            let up = if y0' > y1' then "╰" else "╭"
            result [rows - y1'] [toInteger x + toInteger offset] up

            let down = if y0' > y1' then "╮" else "╯"
            result [rows - y0'] [toInteger x + toInteger offset] down

            let start = minimum [y0', y1'] + 1
            let end = maximum [y0', y1']

            forM_ [start..end] $ \y ->
                result [rows - y] [toInteger x + offset] "│"

    e <- getElems arr
    let result = chunksOf (width + 1) e
    mapM_ (putStrLn . flatten) result

flatten :: [[a]] -> [a]
flatten = flip foldr [] $ flip $ foldr (:)

padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

main :: IO()
main = chart [1,1,1,1,1,2,2,2,2,3,3,3,4,4,4,4,4,3,3,3,3,3,2,2,2,1,1,1,4,4,4,4,20,20,20,20,20,20]

