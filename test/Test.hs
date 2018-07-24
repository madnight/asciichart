{-# LANGUAGE CPP #-}

import Control.Monad   (forM_, replicateM)
import Data.Text.Chart (plot, plotWith, options, height)
import System.Random   (randomRIO)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid         (mempty)
#endif

test :: Int -> IO [Integer]
test = ($ randomRIO (-99999, 99999)) . replicateM

main :: IO ()
main =
  forM_ [0..100] . const $ do
    randLen <- randomRIO (2, 140)
    plot =<< test randLen
    putStrLn mempty
