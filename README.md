# th-format [![Hackage version](https://img.shields.io/hackage/v/th-format.svg?label=Hackage)](https://hackage.haskell.org/package/th-format) [![Stackage version](https://www.stackage.org/package/th-format/badge/lts?label=Stackage)](https://www.stackage.org/package/th-format) [![Build Status](https://travis-ci.org/mtesseract/th-format.svg?branch=master)](https://travis-ci.org/mtesseract/th-format)

### About

This is `th-format`, a Haskell package implementing support for format
strings using Template Haskell quasi quoters. It requires the GHC
extension `QuasiQuotes` to be enabled. Parsing is implemented using
Earley.

This package is BSD3 licensed.

### Examples

Using `th-format`, you can use naive variable interpolation instead of
verbosely concatenating strings manually. Thus, instead of

```haskell
putStrLn $ "Client \"" ++ show client ++ "\" has requested resource \"" ++ show resource ++ "\" at date " ++ show date ++ "."
```

one can directly write:

```haskell
putStrLn $ [fmt|Client "$client" has requested resource "$resource" at date $date|]
```

There are currently two supported ways of interpolation:

1. Simple interpolation, as in `[fmt|Variable foo contains $foo|]`.
1. Expression interpolation, as in `[fmt|The toggle is ${if toggle then ("on" :: Text) else "off"}|]`
