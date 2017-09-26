module Main.Tokens
where

import Control.Alt (void, (<|>))
import Data.Char.Unicode (isSpace)
import Data.List (many)
import Main.AST (genericParser)
import Prelude (Unit, bind, pure, unit, ($), (<*), (==), (||))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (class StringLike, anyChar, char, eof, satisfy)

tok :: forall a. Parser String a -> Parser String a
tok p = p <* (many (void (char ',') <|> genericParser.whiteSpace))

-- whiteSpace :: forall s a. StringLike s => Parser s Unit
-- whiteSpace = void $ many (space <|> comment)
--     where
--         space = void $ satisfy (\c -> isSpace c || c == ',')
--         comment = do
--             _ <- char '#'
--             _ <- manyTill anyChar eol
--             pure unit

eol :: forall s a. StringLike s => Parser s Unit
eol = eof <|> void (char '\n' <|> char '\r')