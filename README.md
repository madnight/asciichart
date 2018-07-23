# WORK IN PROGRESS

# asciichart
[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

ASCII line charts in terminal ╭┈╯ for Haskell (adaption from kroitor/asciichart)
Console ASCII line charts in pure Haskell. This code is absolutely free for any usage (MIT License).

![](example.png)

## Usage
```bash
cabal install asciichart # will be on hackage soon (no available yet)
```

```haskell
import Data.Text.Chart (plot)

main :: IO ()
main = plot [1..20]
```

For more examples e.g. sinus wave see [example folder](https://github.com/madnight/asciichart/tree/master/example).

## References

Full credits to kroitor https://github.com/kroitor/asciichart the inventor of asciichart for the terminal.  
This is only a simple adaptation for the Haskell community.
