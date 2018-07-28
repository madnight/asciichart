{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.Text.Chart
    ( plot
    , plotWith
    , options
    , height
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative     ((<$>))
import Control.Monad.ST.Safe   (ST, runST)
#else
import Control.Monad.ST        (ST, runST)
#endif
import Control.Monad           (forM_)
import Data.Array.ST.Safe      (STArray, getElems, writeArray, newArray)
import Data.Char               (isSpace)
import Data.List               (unfoldr, dropWhileEnd)
import Text.Printf             (printf)
import Data.Bool               (bool)

data Options =
  Options { height :: Int }

-- default options
options :: Options
options =
  Options { height = 14 }

newArray2D :: Integer -> Integer ->
              ST s (STArray s (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

pad :: Integral a => [a] -> Int
pad series =
  let floats = fromIntegral <$> series
      toStr :: [Float] -> [String]
      toStr = fmap (printf "%0.2f")
  in  maximum $ length <$> toStr floats

plotWith' :: Options -> [Integer] -> [String]
plotWith' options series =

    -- variables and functions
    let min' = minimum series
        max' = maximum series
        range = abs $ max' - min'
        offset = 3
        ratio = fromIntegral (height options) / fromIntegral range :: Float
        min2 = fromIntegral min' * ratio
        max2 = fromIntegral max' * ratio
        rows = round $ abs $ max2 - min2
        width = toInteger $ length series + 3

    in runST $ do

    -- array creation
    arr <- newArray2D rows width
    let result [x] [y] = writeArray arr (x, y)

    -- axis and labels
    forM_ [min2..max2] $ \y -> do
            let label = fromInteger max' - (y - min2)
                      * fromInteger range / fromIntegral rows
            result [round $ y - min2] [max 0 $ offset - 5] $
                   printf ("%" ++ show (pad series) ++ ".2f") label
            result [round $ y - min2] [offset - 1] . bool "┤" "┼" $ y == 0

    -- initial value
    let first = fromInteger (head series) * ratio - min2
    result [round $ fromInteger rows - first] [offset - 1] "┼"

    -- plot the line
    forM_ [0..length series - 2] $ \x -> do
        let offset' = toInteger x + offset
        let y i = round (fromInteger (series !! i) * ratio) - round min2
        let (y0, y1) = (y x, y $ x + 1)
        if y0 == y1 then
            result [rows - y0] [offset'] "─"
        else do
            result [rows - y1] [offset'] . bool "╭" "╰" $ y0 > y1
            result [rows - y0] [offset'] . bool "╯" "╮" $ y0 > y1

            forM_ [min y0 y1 + 1..max y0 y1 - 1] $ \y ->
                result [rows - y] [offset'] "│"

    getElems arr

plot :: [Integer] -> IO ()
plot = plotWith options

plotWith :: Options -> [Integer] -> IO ()
plotWith options series = forM_ result $
      putStrLn . dropWhileEnd isSpace . concat
    where result = splitEvery (length series + 4) $ plotWith' options series
