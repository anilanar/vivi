module Language.Protobuf.Types
	( module Data.List
	, ProtoParserT
	, Options
	, Syntax(..)
	, runProtoParser
	)
where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(..), concatMap, fromFoldable, many, some, (:))
import Data.Newtype (unwrap)
import Prelude (($))
import Text.Parsing.Parser (ParseError, ParserT, runParserT)

data Syntax = Syntax2 | Syntax3
type Options = { version :: Syntax }

type ProtoParserT = ParserT String (ReaderT Options Identity)

runProtoParser :: forall a. Options -> ProtoParserT a -> String -> Either ParseError a
runProtoParser options parser text = unwrap
	$ (runReaderT $ runParserT text parser)
	$ options
