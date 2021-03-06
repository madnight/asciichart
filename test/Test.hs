{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad       (forM_, replicateM)
import Data.Text.Chart     (plot, plotWith, options, height)
import System.IO.Silently
import System.Random       (randomRIO)
import Test.Hspec
import Text.RawString.QQ

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid         (mempty)
#endif

randomList :: Int -> IO [Integer]
randomList = ($ randomRIO (-99999, 99999)) . replicateM

test0 :: IO ()
test0 = plotWith options {height = 5} ([1..6] ++ [6,5..1])

test0Output :: String
test0Output = tail [r|
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
test1Output = tail [r|
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
test2Output = tail [r|
 5.00 ┼──╮         ╭────╮         ╭──
 0.00 ┼  ╰──╮    ╭─╯    ╰──╮   ╭──╯
-5.00 ┤     ╰────╯         ╰───╯
|]

test3 :: IO ()
test3 = plot $ take 51 $ cycle [-8975789655001, 6755678990773]

test3Output :: String
test3Output = tail [r|
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

btcChart :: [Float]
btcChart =
    [2591.22,2720.08,2723.58,3255,3445.28,4382.74,4159.46,4137.67           ] ++
    [4387.46,4630.73,4631.69,4638.1,4188.84,3686.9,3897,3777.29,4208.56     ] ++
    [4394.64,4322.75,4772.97,5640.13,5595.23,6013.23,5733.9,6140.53,7030    ] ++
    [6958.21,6570.31,6598.77,7776.94,8230.69,9326.59,9916.54,11616.85       ] ++
    [16057.14,17178.1,19343.04,16454.72,13975.44,14428.76,13412.44,16937.17 ] ++
    [14439.47,14188.78,11141.25,11522.86,11137.24,11158.39,8827.63,7700.39  ] ++
    [8556.61,9477.84,10396.63,9830.43,10313.08,11019.52,10709.53,8787.16    ] ++
    [8196.9,8196.02,8712.89,8138.34,6844.32,7417.89,6896.28,6939.55,8357.04 ] ++
    [8273.74,8938.3,8978.33                                                 ]

test4 :: IO ()
test4 = plot $ round <$> btcChart

test4Output :: String
test4Output = tail [r|
19343.00 ┤                                   ╭╮
18146.43 ┤                                   ││
16949.86 ┤                                  ╭╯╰╮  ╭╮
15753.28 ┤                                 ╭╯  │  ││
14556.72 ┤                                 │   ╰─╮│╰─╮
13360.14 ┤                                 │     ╰╯  │
12163.57 ┤                                ╭╯         │╭╮
10967.00 ┤                                │          ╰╯╰─╮   ╭╮╭──╮
 9770.43 ┤                              ╭─╯              │  ╭╯╰╯  │           ╭
 8573.86 ┤                             ╭╯                ╰╮╭╯     ╰────╮   ╭──╯
 7377.29 ┤                        ╭─╮╭─╯                  ╰╯           ╰───╯
 6180.71 ┤                   ╭────╯ ╰╯
 4984.14 ┤    ╭╮ ╭────╮  ╭───╯
 3787.57 ┤  ╭─╯╰─╯    ╰──╯
 2591.00 ┼──╯
|]

check :: IO a -> String -> IO ()
check test out = do
    (output, _) <- capture test
    output `shouldBe` out

main :: IO ()
main = hspec $ do

    describe "Test 0" $
      it "{height = 5} ([1..6] ++ [6,5..1])" $
        check test0 test0Output

    describe "Test 1" $
      it "{height = 8} sin wave" $
        check test1 test1Output

    describe "Test 2" $
      it "{height = 2} mini cos wave" $
        check test2 test2Output

    describe "Test 3" $
      it "very large numbers" $
        check test3 test3Output

    describe "Test 4" $
      it "bitcoin chart" $
        check test4 test4Output

    describe "Test 5" $
      it "500 random charts" $
      forM_ [1..500] . const $ do
        randLen <- randomRIO (2, 140)
        plot =<< randomList randLen
