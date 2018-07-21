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
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) "  "

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

    forM_ [min2..max2] $ \y -> do
            let label = show (max' - (y - min2) * range `div` rows)
            writeArray arr (y - min2, maximum [offset - 5, 0]) (padL 2 label)
            writeArray arr (y - min2, offset - 1) (if y == 0 then "┼" else "┤")

    let first = head series * ratio - min2
    writeArray arr (rows - first, offset - 1) "┼"

    forM_ [0..(length series - 2)] $ \x -> do

        let y0' = (series !! (x + 0) * ratio) - min2
        let y1' = (series !! (x + 1) * ratio) - min2

        if y0' == y1' then
            writeArray arr
            (rows - y0', toInteger x + toInteger offset)
            "─"
        else do
            writeArray arr
                (rows - y1', toInteger x + toInteger offset)
                (if y0' > y1' then "╰" else "╭")
            writeArray arr
                (rows - y0', toInteger x + toInteger offset)
                (if y0' > y1' then "╮" else "╯")
            let from = minimum [y0', y1']
            let to = maximum [y0', y1']

            forM_ [(from + 1)..to] $ \y ->
                writeArray arr (rows - y, toInteger x + offset) "│"

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
main = chart [1,2,3,5,3,6,7,9,10,11,12,8,13,13,16,16,16,17,18,19,20,20,20,20,18]
