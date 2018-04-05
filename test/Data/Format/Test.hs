{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Format.Test (tests) where

import           Data.Format
import           Data.Text        (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests =
  let foo = "FOO" :: String
      toggle = True
  in testGroup "Tests"
     [ testCase "Empty string" ([fmt||] @=? "")
     , testCase "Single identifier interpolation" ([fmt|$foo|] @=? "FOO")
     , testCase "Multiple identifier interpolation" ([fmt|$foo$foo|] @=? "FOOFOO")
     , testCase "Constant string" ([fmt|Hello, world!|] @=? "Hello, world!")
     , testCase "Constant string w/ escaping" ([fmt|How much \$\$ do you need?|] @=? "How much $$ do you need?")
     , testCase "Constant string w/ escaping" ([fmt|Got \$\$\? :-\\|] @=? "Got $$? :-\\")
     , testCase "Delimited interpolation" ([fmt|${foo}|] @=? "FOO")
     , testCase "Multiple delimited interpolations" ([fmt|${foo}${foo}|] @=? "FOOFOO")
     , testCase "Mixed interpolations" ([fmt|${foo}$foo${foo}|] @=? "FOOFOOFOO")
     , testCase "Single interpolation between text" ([fmt|Hey $foo, how ya doing?|] @=? "Hey FOO, how ya doing?")
     , testCase "Single interpolation between text w/o ws" ([fmt|BAR$foo|] @=? "BARFOO")
     , testCase "Delimited interpolation between text" ([fmt|BAR${foo}BAR|] @=? "BARFOOBAR")
     , testCase "Delimited interpolation between text" ([fmt|BAR${foo}BAR|] @=? "BARFOOBAR")
     , testCase "Escaped simple interpolation" ([fmt|Escaped simple interpolation: \$foo|] @=? "Escaped simple interpolation: $foo")
     , testCase "Escaped delimited interpolation" ([fmt|Escaped delimited interpolation: \${foo}|] @=? "Escaped delimited interpolation: ${foo}")
     , testCase "Simple interpolations delimited by ws" ([fmt|$foo $foo|] @=? "FOO FOO")
     , testCase "Boolean toggle interpolation" (([fmt|${if toggle then ("on" :: Text) else "off"}|] :: Text) @=? "on")
     , testCase "Boolean toggle interpolation negated" (([fmt|${if (not toggle) then ("on" :: Text) else "off"}|] :: Text) @=? "off")
     , testCase "Code interpolation surrounded by text" (([fmt|Hello ${mconcat ["hi", " ", "hi"] :: Text}!|]) @=? "Hello hi hi!")
     ]
