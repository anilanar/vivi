module Main.Tokens
where

import Control.Alt (void, (<$>), (<|>))
import Control.Plus (empty)
import Data.Array (fromFoldable)
import Data.Char.Unicode (isSpace)
import Data.List.Lazy (List(..), many, some)
import Data.String (fromCharArray)
import Main.AST (Name(..))
import Main.Types (SParser)
import Prelude (class Monad, Unit, bind, pure, unit, ($), (<*), (<*>), (<<<), (<>), (==), (>>>), (||))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String (class StringLike, anyChar, char, eof, satisfy)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), GenTokenParser, alphaNum, letter, makeTokenParser)

tok :: forall a. Parser String a -> Parser String a
tok p = do
	result <- p
	_ <- whiteSpace
	pure result

whiteSpace :: forall s a. StringLike s => Parser s Unit
whiteSpace = void $ many (space <|> comment)
	where
		space = void $ satisfy (\c -> isSpace c || c == ',')
		comment = do
			_ <- char '#'
			_ <- manyTill anyChar eol
			pure unit

eol :: forall s a. StringLike s => Parser s Unit
eol = eof <|> void (char '\n' <|> char '\r')

nameParser :: SParser Name
nameParser = Name <$> tok (fromCharArray <<< fromFoldable <$> ((<>) <$> (some (letter <|> char '_')) <*> (many (alphaNum <|> char '_'))))

genericParser :: forall m. Monad m => GenTokenParser String m
genericParser = makeTokenParser $ LanguageDef def
    where
        def =
            { commentStart: ""
            , commentEnd: ""
            , commentLine: "#"
            , nestedComments: false
            , identStart: letter <|> char '_'
            , identLetter: alphaNum <|> char '_'
            , opStart: empty
            , opLetter: empty
            , reservedOpNames: []
            , reservedNames: []
            , caseSensitive: true
            }