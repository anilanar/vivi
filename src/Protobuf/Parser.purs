module Language.Protobuf.Parser
where

import Control.Alt (void, (<$), (<$>), (<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Monoid (class Monoid, mempty, (<>))
import Language.Protobuf.AST as AST
import Language.Protobuf.Tokens (genericParser, identParser, tok)
import Language.Protobuf.Types (List(..), ProtoParserT, concatMap, many, (:))
import Prelude (Unit, bind, const, pure, (*>), (<*), (<*>))
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy, sepBy1)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String (char, string)

document :: ProtoParserT AST.Document
document = AST.Document
	<$ genericParser.whiteSpace
	<* syntax
	<*> manyStatements statement

statement :: ProtoParserT AST.Statement
statement = AST.StatementImport <$> import_
		<|> AST.StatementPackage <$> package
		<|> AST.StatementOption <$> option
		<|> AST.StatementDefinition <$> definition

import_ :: ProtoParserT AST.Import
import_ = AST.Import
	<$ tok (string "import")
	<*> optionMaybe importSpecifier
	<*> (AST.StringLiteral <$> genericParser.stringLiteral)
	<* tok (char ';')

importSpecifier :: ProtoParserT AST.ImportSpecifier
importSpecifier = AST.Weak <$ tok (string "weak")
	<|> AST.Public <$ tok (string "public")

package :: ProtoParserT AST.Package
package = AST.Package
	<$ tok (string "package")
	<*> fullIdent
	<* tok (char ';')

definition :: ProtoParserT AST.Definition
definition = AST.DefinitionMessage <$> message
	<|> AST.DefinitionEnum <$> enum
	<|> AST.DefinitionService <$> service

message :: ProtoParserT AST.Message
message = fix \rec -> AST.Message
	<$ tok (string "message")
	<*> ident
	<*> braces (manyStatements (messageBody rec))

messageBody :: ProtoParserT AST.Message -> ProtoParserT AST.MessageBody
messageBody message' = AST.MessageBodyEnum <$> enum
		<|> AST.MessageBodyMessage <$> message'
		<|> AST.MessageBodyOption <$> option
		<|> AST.MessageBodyOneOf <$> oneOf
		<|> AST.MessageBodyMapField <$> mapField
		<|> AST.MessageBodyReserved <$> reserved
		<|> AST.MessageBodyNormalField <$> normalField

normalField :: ProtoParserT AST.NormalField
normalField = repeated <|> normal
	where
		repeated = AST.FieldNormalRepeated
			<$ tok (string "repeated")
			<*> type_
			<*> ident
			<* tok (char '=')
			<*> fieldNumber
			<*> optempty (brackets (fieldOption `sepBy` tok (char ',')))
			<* tok (char ';')
		normal = AST.FieldNormal
			<$> type_
			<*> ident
			<* tok (char '=')
			<*> fieldNumber
			<*> optempty (brackets (fieldOption `sepBy` tok (char ',')))
			<* tok (char ';')

oneOf :: ProtoParserT AST.OneOf
oneOf = AST.OneOf
	<$ tok (string "oneof")
	<*> ident
	<*> braces (manyStatements oneOfField)

oneOfField :: ProtoParserT AST.OneOfField
oneOfField = AST.OneOfField
	<$> type_
	<*> ident
	<* tok (char '=')
	<*> fieldNumber
	<*> (brackets (many fieldOption))
	<* tok (char ';')

enum :: ProtoParserT AST.Enum
enum = AST.Enum
	<$ tok (string "enum")
	<*> ident
	<*> braces (manyStatements enumBody)

enumBody :: ProtoParserT AST.EnumBody
enumBody = AST.EnumBodyOption <$> option
	<|> AST.EnumBodyField <$> enumField

enumField :: ProtoParserT AST.EnumField
enumField = AST.EnumField
	<$> ident
	<* tok (char '=')
	<*> genericParser.integer
	<*> optempty (brackets (enumValueOption `sepBy` tok (char ',')))
	<* tok (char ';')

enumValueOption :: ProtoParserT AST.EnumValueOption
enumValueOption = AST.EnumValueOption
	<$> optionName
	<* tok (char '=')
	<*> constant

service :: ProtoParserT AST.Service
service = AST.Service
	<$ tok (string "service")
	<*> ident
	<*> braces (manyStatements serviceBody)

serviceBody :: ProtoParserT AST.ServiceBody
serviceBody = AST.ServiceBodyOption <$> option
	<|> AST.ServiceBodyRpc <$> rpc

rpc :: ProtoParserT AST.Rpc
rpc = AST.Rpc
	<$ tok (string "rpc")
	<*> ident
	<*> parens rpcType
	<* tok (string "returns")
	<*> parens rpcType
	<*> optionsOrEnd
	where
		rpcType :: ProtoParserT AST.RpcType
		rpcType = AST.RpcTypeStream <$ (tok (string "stream")) <*> messageType
			<|> AST.RpcType <$> messageType
		optionsOrEnd = braces (manyStatements option)
			<|> (const mempty) <$> tok (char ';')

option :: ProtoParserT AST.Option
option = AST.Option
	<$ tok (string "option")
	<*> optionName
	<* tok (char '=')
	<*> constant
	<* tok (char ';')

optionName :: ProtoParserT AST.OptionName
optionName = AST.Customized
	<$> parens fullIdent
	<*> optempty (tok (char '.') *> ident `sepBy` (tok (char '.')))
	<|> AST.Predefined <$> ident

mapField :: ProtoParserT AST.MapField
mapField = AST.MapField
	<$ tok (string "map<")
	<*> keyType
	<* tok (char ',')
	<*> type_
	<* tok (char '>')
	<*> ident
	<* tok (char '=')
	<*> fieldNumber
	<*> optempty (brackets (fieldOption `sepBy` tok (char ',')))

reserved :: ProtoParserT AST.Reserved
reserved = (reservedRanges <|> reservedNames)
	<* tok (char ';')
	where
		reservedToken = tok (string "reserved")
		reservedRanges = AST.ReservedRanges
			<$ reservedToken
			<*> range `sepBy1` tok (char ',')
			<* tok (char ';')
		reservedNames = AST.ReservedNames
			<$ reservedToken
			<*> ident `sepBy1` tok (char ',')

fieldOption :: ProtoParserT AST.FieldOption
fieldOption = AST.FieldOption
	<$> ident
	<* tok (char '=')
	<*> constant

range :: ProtoParserT AST.Range
range = AST.Range
	<$> genericParser.integer
	<*> optionMaybe to
	where
		to = tok (string "to") *> intOrMax
		intOrMax = Left <$> genericParser.integer
			<|> Right <$> (AST.Max <$ tok (string "max"))

fieldNumber :: ProtoParserT AST.FieldNumber
fieldNumber = AST.FieldNumber <$> genericParser.integer

type_ :: ProtoParserT AST.Type
type_ = AST.Type <$> messageTypeParser

messageType :: ProtoParserT AST.Ident
messageType = AST.Ident <$> messageTypeParser

messageTypeParser :: ProtoParserT String
messageTypeParser = do
	dotPrefix <- C.option "" (tok (string "."))
	body <- foldl (<>) "" <$> rest
	pure (dotPrefix <> body)
	where
		rest = identParser `sepBy` tok (char '.')

keyType :: ProtoParserT AST.KeyType
keyType = AST.Double <$ tok (string "double")
	<|> AST.Float <$ tok (string "float")
	<|> AST.Int32 <$ tok (string "int32")
	<|> AST.Int64 <$ tok (string "int64")
	<|> AST.UInt32 <$ tok (string "uint32")
	<|> AST.UInt64 <$ tok (string "uint64")
	<|> AST.SInt32 <$ tok (string "sint32")
	<|> AST.SInt64 <$ tok (string "sint64")
	<|> AST.Fixed32 <$ tok (string "fixed32")
	<|> AST.Fixed64 <$ tok (string "fixed64")
	<|> AST.SFixed32 <$ tok (string "sfixed32")
	<|> AST.SFixed64 <$ tok (string "sfixed64")
	<|> AST.Bool <$ tok (string "bool")
	<|> AST.String <$ tok (string "string")

constant :: ProtoParserT AST.Constant
constant = AST.ConstantIdent <$> fullIdent
	<|> AST.ConstantInt <$> genericParser.integer
	<|> AST.ConstantFloat <$> genericParser.float
	<|> AST.ConstantString <$> stringLiteral
	<|> AST.ConstantBool <$> boolLiteral

manyStatements :: forall a. ProtoParserT a -> ProtoParserT (List a)
manyStatements p = filterEmpty <$> many withEmpty
	where
		withEmpty :: ProtoParserT (Either a Unit)
		withEmpty = Right <$> emptyStatement
			<|> (Left <$> p)

filterEmpty :: forall a. List (Either a Unit) -> List a
filterEmpty = concatMap toList
	where toList e = case e of
		Left x -> x : Nil
		Right _ -> Nil


stringLiteral :: ProtoParserT AST.StringLiteral
stringLiteral = AST.StringLiteral <$> genericParser.stringLiteral

boolLiteral :: ProtoParserT Boolean
boolLiteral = true <$ tok (string "true")
	<|> false <$ tok (string "false")

emptyStatement :: ProtoParserT Unit
emptyStatement = void (tok (char ';'))

syntax :: ProtoParserT Unit
syntax = void syntaxParser
	where
		syntaxParser = tok (string "syntax")
			<* tok (char '=')
			<* tok (string "\"proto3\"")
			<* tok (char ';')

fullIdent :: ProtoParserT AST.FullIdent
fullIdent = AST.FullIdent <$> (ident `sepBy1` char '.')

ident :: ProtoParserT AST.Ident
ident = AST.Ident <$> identParser

optempty :: forall a. Monoid a => ProtoParserT a -> ProtoParserT a
optempty = C.option mempty

parens :: forall a. ProtoParserT a -> ProtoParserT a
parens = between (char '(') (char ')')

braces :: forall a. ProtoParserT a -> ProtoParserT a
braces = between (char '{') (char '}')

brackets :: forall a. ProtoParserT a -> ProtoParserT a
brackets = between (char '[') (char ']')

between :: forall a b c. ProtoParserT a -> ProtoParserT b -> ProtoParserT c -> ProtoParserT c
between open close p = tok open *> p <* tok close