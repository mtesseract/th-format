{-|
Module      : Data.Format
Description : QuasiQuoters for simple string interpolation.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Format
  ( fmt
  , fmtConcat
  , Format(..)
  ) where

import           Control.Applicative
import           Control.Exception           (SomeException)
import           Data.Char
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as Text.Lazy
import           Language.Haskell.Meta.Parse
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Earley

-- | This is just specialized 'mconcat', reexported under a
-- specialized name in order to avoid namespace clashes.
fmtConcat :: [Text] -> Text
fmtConcat = mconcat

-- | Type class which needs to be implemented by types that should be
-- usable for format string interpolation. For most types the this
-- class is simply implemented in terms of 'show'. But for
-- human-readable strings (e.g. 'String', 'Text'), the format
-- representation is simply the string itself, not its 'show'-image
-- (which adds quotation characters).
class Format a where
  formatText :: a -> Text

instance Format Int where
  formatText = tshow

instance Format SomeException where
  formatText = tshow

instance Format String where
  formatText = Text.pack

instance Format Double where
  formatText = tshow

instance Format Float where
  formatText = tshow

instance Format Integer where
  formatText = tshow

instance Format Text where
  formatText = id

instance Format Text.Lazy.Text where
  formatText = Text.Lazy.toStrict

instance Format Bool where
  formatText = tshow

tshow :: Show a => a -> Text
tshow = Text.pack . show

data Fmt = Literal String
         | Identifier String
         | Expression String
  deriving (Show, Eq)

-- | Quasi Quoter for format strings. Examples:
--
-- Examples:
--
-- >>> let answer = 42 in [fmt|What is the answer to universe, life and everything? It's $answer!|]
-- "What is the answer to universe, life and everything? It's 42!"
--
-- >>> let toggle = True in [fmt|The toggle is switched ${if toggle then ("on" :: Text) else "off"}|]
-- "The toggle is switched on"
--
-- >>> let timeDelta = 60 in [fmt|Request latency: ${timeDelta}ms|]
-- "Request latency: 60ms"
fmt :: QuasiQuoter
fmt = QuasiQuoter { quoteExp = parseFormatStringQ
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec = undefined
                  }

instance Lift Fmt where
  lift (Literal s)    = stringE s
  lift (Identifier s) =
    lookupValueName s >>= \case
      Just v  -> (return . formatTextEmbed . VarE) v
      Nothing -> fail $ "Not in scope: '" ++ s ++ "'"
  lift (Expression s) = either fail (return . formatTextEmbed) (parseExp s)

formatTextEmbed :: Exp -> Exp
formatTextEmbed expr = AppE (VarE 'formatText) expr

newtype FmtString = FmtString [Fmt]

instance Lift FmtString where
  lift (FmtString fmts) = do
    fmtExprs <- Prelude.mapM lift fmts
    return $ AppE (VarE 'fmtConcat) (ListE fmtExprs)

-- | Parse the provided format string as a Template Haskell
-- expression.
parseFormatStringQ :: String -> Q Exp
parseFormatStringQ s =
  let parseResult = FmtString (parseFormatString s)
  in  [| parseResult |]

-- | Parse the provided format string as a list of 'Fmt' values.
parseFormatString :: String -> [Fmt]
parseFormatString s =
  case fullParses (parser fmtParser) s of
    ([], Report { unconsumed = "" }) ->
      []
    ([uniqueResult], Report { unconsumed = "" }) ->
      uniqueResult
    _ ->
      fail "Parse failure"

-- | Earley parser for the grammar of format strings.
fmtParser :: Grammar r (Prod r String Char [Fmt])
fmtParser = mdo
  -- Initial rule.
  start <- rule $ interpolationOrLiteral

  -- Either parse an interpolation or a non-empty string literal next.
  interpolationOrLiteral <- rule $
    interpolationThenRest
    <|> literalThenRest

  -- Parse an interpolation next (either `$foo$` or `${foo}`).
  interpolationThenRest <- rule $
    interpolationSimpleThenRest
    <|> interpolationDelimitedThenRest

  -- Parse a simple interpolation next (i.e. `$foo`).
  interpolationSimpleThenRest <- rule $
    (Identifier <$> interpolationSimple) `apCons` delimLiteralThenRest
    <|> (Identifier <$> interpolationSimple) `apCons` interpolationThenRest
    <|> (Identifier <$> interpolationSimple) `apCons` pure []

  -- Parse a delimited interpolation next (i.e. `${foo}`).
  interpolationDelimitedThenRest <- rule $
    (Expression <$> interpolationDelimited) `apCons` interpolationOrLiteral
    <|> (Expression <$> interpolationDelimited) `apCons` pure []

  -- Parse a single character literal which marks the beginning of a
  -- string literal and can be used to end a previous simple
  -- interpolation (e.g. whitspace, comma).
  delimLiteral <- rule $ Literal <$>
    (satisfy (\c -> not (identifierChar c) && c /= '$')) `apCons` strChars

  -- Parse a string literal next which starts with a delimiting character.
  delimLiteralThenRest <- rule $
    delimLiteral `apCons` interpolationThenRest
    <|> delimLiteral `apCons` (pure [])

  -- Parse a string literal next.
  literalThenRest <- rule $
    (Literal <$> literal) `apCons` pure []
    <|> (Literal <$> literal) `apCons` interpolationThenRest

  -- Parse a single Haskell variable name next.
  identifier <- rule $
    satisfy initialIdentifierChar `apCons` many (satisfy identifierChar)

  -- Parse a simple interpolation next.
  interpolationSimple <- rule $ token '$' *> identifier

  -- Parse a delimited interpolation next.
  interpolationDelimited <- rule $ token '$' *> token '{' *> expression <* token '}'

  -- Parses a single string literal character. Supports escaping.
  strChar <- rule $
    satisfy (`Prelude.notElem` ['$', '\\'])
    <|> token '\\' *> satisfy (const True)

  -- Possibly potentially empty string literal.
  strChars <- rule $ many strChar

  -- Nonempty string literal
  literal <- rule $ strChar `apCons` strChars

  -- Parse a expression, i.e. something contained between "${" and "}".
  expression <- rule $ some (satisfy (/= '}'))

  return start

  where apCons = liftA2 (:)

-- | Return True if the given character can be part of a Haskell
-- variable name, False otherwise.
identifierChar :: Char -> Bool
identifierChar c = isLower c || isUpper c || c `Prelude.elem` ['\'', '_']

-- | Return True if the given character can be the initial character
-- of a Haskell variable name.
initialIdentifierChar :: Char -> Bool
initialIdentifierChar c = isLower c || c == '_'
