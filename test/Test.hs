{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad       (forM_, replicateM)
import Data.Text.Chart     (plot, plotWith, options, height)
import Test.Hspec
import Text.RawString.QQ
import System.IO.Silently
import System.Random       (randomRIO)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid         (mempty)
#endif

randomList :: Int -> IO [Integer]
randomList = ($ randomRIO (-99999, 99999)) . replicateM

test0 :: IO ()
test0 = plotWith options {height = 5} ([1..6] ++ [6,5..1])

test0Output :: String
test0Output = [r|
6.00 ┤    ╭─╮
5.00 ┤   ╭╯ ╰╮
4.00 ┤  ╭╯   ╰╮
3.00 ┤ ╭╯     ╰╮
2.00 ┤╭╯       ╰╮
1.00 ┼╯         ╰
|]

test1 :: IO ()
test1 = plotWith options {height = 8} wave
  where wave = round . (5 *) . sin . (/ 120) . (pi *) . (4 *) <$> [0..60]

test1Output :: String
test1Output = [r|
 5.00 ┤          ╭────────╮
 3.75 ┤       ╭──╯        ╰──╮
 2.50 ┤  ╭────╯              ╰────╮
 1.25 ┤╭─╯                        ╰─╮
 0.00 ┼╯                            ╰╮                            ╭
-1.25 ┤                              ╰─╮                        ╭─╯
-2.50 ┤                                ╰────╮              ╭────╯
-3.75 ┤                                     ╰──╮        ╭──╯
-5.00 ┤                                        ╰────────╯
|]

test2 :: IO ()
test2 = plotWith options {height = 2} wave
  where wave = round . (5 *) . cos . (/ 30) . (pi *) . (4 *) <$> [0..30]

test2Output :: String
test2Output = [r|
 5.00 ┼──╮         ╭────╮         ╭──
 0.00 ┼  ╰──╮    ╭─╯    ╰──╮   ╭──╯
-5.00 ┤     ╰────╯         ╰───╯
|]

test3 :: IO ()
test3 = plot $ take 51 $ cycle [-8975789655001, 6755678990773]

test3Output :: String
test3Output = [r|
 6755679000000.00 ┤╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮╭╮
 5632002400000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
 4508326300000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
 3384649700000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
 2260973600000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
 1137297000000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
   13620478000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-1110055600000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-2233731600000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-3357408800000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-4481085000000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-5604761000000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-6728438000000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-7852114000000.00 ┤││││││││││││││││││││││││││││││││││││││││││││││││││
-8975790000000.00 ┼╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰╯╰
|]

main :: IO ()
main = hspec $ do

    describe "Test 0" $
      it "{height = 5} ([1..6] ++ [6,5..1])" $ do
      (output, _) <- capture test0
      "\n" ++ output `shouldBe` test0Output

    describe "Test 1" $
      it "{height = 8} sin wave" $ do
      (output, _) <- capture test1
      "\n" ++ output `shouldBe` test1Output

    describe "Test 2" $
      it "{height = 2} mini cos wave" $ do
      (output, _) <- capture test2
      "\n" ++ output `shouldBe` test2Output

    describe "Test 3" $
      it "very large numbers" $ do
      (output, _) <- capture test3
      "\n" ++ output `shouldBe` test3Output

    describe "Test 4" $
      it "500 random charts" $
      forM_ [1..500] . const $ do
        randLen <- randomRIO (2, 140)
        plot =<< randomList randLen
