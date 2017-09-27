module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Main.Parser (parse)
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert')
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (class StringLike, eof)

main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  --parseTest " \n\r #comment hehe \n #kjn\n#abc  " unit whiteSpace
  --parseTest "\n" unit whiteSpace
  log gqlExample
  parseShow gqlExample parse

gqlExample =
  "type Character {\n\
  \  name: String!\n\
  \  appearsIn: [Episode]!\n\
  \}"

parseTest :: forall s a eff
  .  Show a
  => Eq a
  => StringLike s
  => s -> a -> Parser s a
  -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p = case runParser input (p <* eof) of
  Right actual -> do
    assert'
      ("expected: " <> show expected <> ", actual: " <> show actual)
      (expected == actual)
  Left err -> assert' ("error: " <> show err) false

parseShow :: forall s a eff
  .  Show a
  => StringLike s
  => s -> Parser s a
  -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseShow input p = case runParser input (p <* eof) of
  Right actual -> do
    logShow ""
  Left err -> assert' ("error: " <> show err) false