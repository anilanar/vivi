module Test.Util
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Language.GraphQL.Types (PP)
import Prelude (class Eq, class Show, Unit, show, (<*), (<>), (==))
import Test.Assert (ASSERT, assert, assert')
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

parseTest :: forall s a eff
  .  Show a
  => Eq a
  => String -> a -> PP a
  -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p = case runParser input (p <* eof) of
  Right actual -> do
	assert'
	  ("expected: " <> show expected <> ", actual: " <> show actual)
	  (expected == actual)
  Left err -> assert' ("error: " <> show err) false

parseTest' :: forall s a eff
  .  Show a
  => String -> PP a
  -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest' input p = case runParser input (p <* eof) of
  Right actual -> do
	assert' ("expected: error" <> ", actual: " <> show actual) false
  Left err -> assert true