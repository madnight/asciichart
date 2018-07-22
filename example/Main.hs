import Data.Text.Chart (plot)

main :: IO ()
main = do

  plot [1..20]

  -- sinus example
  plot $ map (\i -> round $ 10 * sin (i * (pi * 4) / 120) ) [0..120]
