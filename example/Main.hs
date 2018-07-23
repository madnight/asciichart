import Data.Text.Chart (plot)

main :: IO ()
main = do

  plot [1..20]

  -- sinus example
  plot $ round . (10 *) . sin . (/ 120) . (pi *) . (4 *) <$> [0..120]
