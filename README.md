# asciichart

<p><a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-brightgreen.svg" alt="License: MIT" /></a>
<a href="https://travis-ci.org/madnight/asciichart"><img src="https://travis-ci.org/madnight/asciichart.svg?branch=master" alt="Build Status" /></a>
<img src="https://badgen.net/badge/icon/haskell?icon=haskell&label&color=purple" alt="" />
</a></p>


ASCII line charts in terminal ╭┈╯. Console line charts in pure Haskell.  
This is a Haskell port of the Javascript library [kroitor/asciichart](https://github.com/kroitor/asciichart). Free for any usage (MIT License).

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
You can also find this package on [Hackage](http://hackage.haskell.org/package/asciichart).

## References

Full credits to [kroitor](https://github.com/kroitor/) the inventor of asciichart for the terminal.  
This is only a simple port for the Haskell community.
