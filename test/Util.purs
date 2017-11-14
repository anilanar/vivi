module Test.Util
where

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Language.GraphQL.Types (PP)
import Prelude (class Eq, Unit, discard, pure, show, ($), (<*), (==))
import Test.Spec.Assertions (fail, shouldEqual)
import Text.Parsing.Parser (ParseError(..), runParser)
import Text.Parsing.Parser.String (eof)

parseTest :: forall a r
    .  Eq a
    => String -> a -> PP a -> Aff r Unit
parseTest input expected p = case runParser input (p <* eof) of
    Right actual -> (actual == expected) `shouldEqual` true
    Left err -> fail $ show err

parseTest' :: forall a r. String -> PP a -> Aff r String
parseTest' input p = case runParser input (p <* eof) of
    Right _ -> do
        fail $ "Parse should have failed"
        pure ""
    Left (ParseError err _) -> pure err