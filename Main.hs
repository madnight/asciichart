
import Control.Lens

import Control.Monad.ST
import Control.Monad (forM_)
import Data.Array.IO
import Data.List.Split
import Data.List (intercalate)
import System.Environment
import Data.List
import Data.Char
import Data.Maybe


show' :: Show a => [a] -> String
show' = intercalate " " . map show

chart :: [Integer] -> IO()
chart series = do
    let min' = minimum series
    let max' = maximum series
    let range = abs $ max' - min'
    let offset = 3
    {- let ratio = range `div` range -}
    let ratio = 1
    {- let min2 = round $ min * ratio -}
    let min2 = min' * ratio
    {- let max2 = round $ max * ratio -}
    let max2 =  max' * ratio
    let rows = abs $ max2 - min2
    let width = length series + 3
    arr <- newArray ((0,0), (rows, (toInteger width))) "  " :: IO (IOArray (Integer, Integer) String)
    forM_ [min2..max2] $ \y -> do
            let label = show (max' - (y - min2) * range `div` rows)
            writeArray arr ((y - min2), (maximum [(offset - 5), 0])) (padL 2 label)
            writeArray arr ((y - min2), (offset - 1)) (if y == 0 then "┼" else "┤")
    e <- getElems arr

    let result = chunksOf (width+1) $ e
        {- b <- buildPair rows (toInteger width) -}
        {- a <- buildPair rows (toInteger width) -}
        {- return b -}
    {- let label y = show (max' - (y - min2) * range `div` rows) -}
    {- let set x y list value = list & element x . element y .~ value -}
    {- let l1 y list = set (fromInteger (y - min2)) (maximum [(offset - 0), 0]) list (label y) -}
    {- let l2 y list = set (fromInteger (y - min2)) (offset - 1) list (if y == 0 then '┼' else '┤') -}
    {- let assign y z = if y <= max2 then z else assign (y + 1) (l2 y (l1 y z)) -}
    {- let assign y z = if y <= max2 then z else assign (y + 1) ((l1 y z)) -}
    {- fmap (\i -> ) [min2..max2] -}

    {- let y0 = (head series * ratio) - min2 -}
    {- let res1 = assign min2 (result) -}
    {- let res2 = set (fromInteger (rows - y0)) (offset -1) res1 "┼" -}
    {- print $ res2 -}
    {- print $ map show' result -}
    _ <- (mapM) (putStrLn . flatten) result
    return ()


flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

main :: IO()
main = chart [1..20]

padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s
