# asciichart
[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/madnight/asciichart.svg?branch=master)](https://travis-ci.org/madnight/asciichart)

ASCII line charts in terminal ╭┈╯. Haskell port of kroitor/asciichart.
Console line charts in pure Haskell. This code is absolutely free for any usage (MIT License).

![](example.png)

## Usage
```bash
cabal install asciichart
```

```haskell
import Data.Text.Chart (plot)

main :: IO ()
main = plot [1..20]
```

For more examples e.g. sinus wave see [examples folder](https://github.com/madnight/asciichart/tree/master/examples).

## References

Full credits to kroitor https://github.com/kroitor/asciichart the inventor of asciichart for the terminal.  
This is only a simple port for the Haskell community.
