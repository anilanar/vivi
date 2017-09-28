module Language.GraphQL.Tokens
where

import Control.Alt (void, (<$>), (<|>))
import Control.Plus (empty)
import Data.Array (fromFoldable)
import Data.Char.Unicode (isSpace)
import Data.String (fromCharArray)
import Language.GraphQL.AST (Name(..))
import Language.GraphQL.Types (PP, many, some)
import Prelude (class Monad, Unit, ($), (<*), (<*>), (<<<), (<>), (==), (||))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (class StringLike, anyChar, char, eof, satisfy)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), GenTokenParser, alphaNum, letter, makeTokenParser)

tok :: forall a. PP a -> PP a
tok p = p <* whiteSpace

whiteSpace :: forall s a. StringLike s => Parser s Unit
whiteSpace = void (many (space <|> comment))
	where
		space = void $ satisfy (\c -> isSpace c || c == ',')
		comment = void (char '#') <* manyTill (void anyChar) eol

eol :: forall s a. StringLike s => Parser s Unit
eol = eof <|> void (char '\n' <|> char '\r')

nameParser :: PP Name
nameParser = Name <$> tok ident
    where
        ident = fromCharArray <<< fromFoldable
            <$> ((<>) <$> (some identStart) <*> (many identCont))
        identStart = char '_' <|> letter
        identCont = char '_' <|> alphaNum

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