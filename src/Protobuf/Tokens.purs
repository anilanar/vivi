module Language.Protobuf.Tokens
where

import Control.Alt (void, (<$>), (<|>))
import Control.Plus (empty)
import Data.Array (fromFoldable)
import Data.String (fromCharArray)
import Language.Protobuf.Types (ProtoParserT, many, some)
import Prelude (class Monad, Unit, ($), (<*), (<*>), (<<<), (<>))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (class StringLike, char, eof)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), GenTokenParser, alphaNum, letter, makeTokenParser)

tok :: forall a. ProtoParserT a -> ProtoParserT a
tok p = p <* genericParser.whiteSpace

eol :: forall s. StringLike s => Parser s Unit
eol = eof <|> void (char '\n' <|> char '\r')

identParser :: ProtoParserT String
identParser = tok ident
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
            , commentLine: "//"
            , nestedComments: false
            , identStart: letter <|> char '_'
            , identLetter: alphaNum <|> char '_'
            , opStart: empty
            , opLetter: empty
            , reservedOpNames: []
            , reservedNames: []
            , caseSensitive: true
            }