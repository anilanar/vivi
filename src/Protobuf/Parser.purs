module Language.Protobuf.Parser
where

import Control.Alt (void, (<$), (<$>), (<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Monoid (class Monoid, mempty, (<>))
import Language.Protobuf.AST as AST
import Language.Protobuf.Tokens (genericParser, identParser, tok)
import Language.Protobuf.Types (PP, List, concatMap, many, nil, (:))
import Prelude (Unit, (*>), (<*), (<*>))
import Text.Parsing.Parser.Combinators (optionMaybe, optional, sepBy, sepBy1)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String (char, string)

document :: PP AST.Document
document = AST.Document
	<$ genericParser.whiteSpace
	<* syntax
	<*> manyStatements statement

statement :: PP AST.Statement
statement = AST.StatementImport <$> import_
		<|> AST.StatementPackage <$> package
		<|> AST.StatementOption <$> option
		<|> AST.StatementDefinition <$> definition

import_ :: PP AST.Import
import_ = AST.Import
	<$ tok (string "import")
	<*> optionMaybe importSpecifier
	<*> (AST.StringLiteral <$> genericParser.stringLiteral)
	<* tok (char ';')

importSpecifier :: PP AST.ImportSpecifier
importSpecifier = AST.Weak <$ tok (string "weak")
	<|> AST.Public <$ tok (string "public")

package :: PP AST.Package
package = AST.Package
	<$ tok (string "package")
	<*> fullIdent
	<* tok (char ';')

option :: PP AST.Option
option = AST.Option
	<$ tok (string "option")
	<*> optionName
	<* tok (char '=')
	<*> constant
	<* tok (char ';')

optionName :: PP AST.OptionName
optionName = AST.Customized <$> parens fullIdent <*> many ident
	<|> AST.Predefined <$> ident

definition :: PP AST.Definition
definition = AST.DefinitionMessage <$> message
	<|> AST.DefinitionEnum <$> enum
	-- <|> AST.DefinitionService <$> service

message :: PP AST.Message
message = fix \rec -> AST.Message
	<$ tok (string "message")
	<*> ident
	<*> braces (manyStatements (messageBody rec))

messageBody :: PP AST.Message -> PP AST.MessageBody
messageBody message = AST.MessageBodyEnum <$> enum
		<|> AST.MessageBodyMessage <$> message
		<|> AST.MessageBodyOption <$> option
		<|> AST.MessageBodyOneOf <$> oneOf
		<|> AST.MessageBodyMapField <$> mapField
		<|> AST.MessageBodyReserved <$> reserved
		<|> AST.MessageBodyNormalField <$> normalField

normalField :: PP AST.NormalField
normalField = repeated <|> normal
	where
		repeated = AST.FieldNormalRepeated
			<$ tok (string "repeated")
			<*> type_
			<*> ident
			<* tok (char '=')
			<*> fieldNumber
			<*> fieldOption `sepBy` tok (char ',')
			<* tok (char ';')
		normal = AST.FieldNormal
			<$> type_
			<*> ident
			<* tok (char '=')
			<*> fieldNumber
			<*> fieldOption `sepBy` tok (char ',')
			<* tok (char ';')

fieldNumber :: PP AST.FieldNumber
fieldNumber = AST.FieldNumber <$> genericParser.integer

fieldOption :: PP AST.FieldOption
fieldOption = AST.FieldOption
	<$> ident
	<* tok (char '=')
	<*> constant

oneOf :: PP AST.OneOf
oneOf = AST.OneOf
	<$ tok (string "oneof")
	<*> ident
	<*> braces (manyStatements oneOfField)

oneOfField :: PP AST.OneOfField
oneOfField = AST.OneOfField
	<$> type_
	<*> ident
	<* tok (char '=')
	<*> fieldNumber
	<*> (brackets (many fieldOption))

enum :: PP AST.Enum
enum = AST.Enum
	<$ tok (string "enum")
	<*> ident
	<*> braces (manyStatements enumBody)

enumBody :: PP AST.EnumBody
enumBody = AST.EnumBodyOption <$> option
	<|> AST.EnumBodyField <$> enumField

enumField :: PP AST.EnumField
enumField = AST.EnumField
	<$> ident
	<* tok (char '=')
	<*> genericParser.integer
	<*> optempty (brackets (enumValueOption `sepBy` tok (char ',')))
	<* tok (char ';')

enumValueOption :: PP AST.EnumValueOption
enumValueOption = AST.EnumValueOption
	<$> optionName
	<* tok (char '=')
	<*> constant

mapField :: PP AST.MapField
mapField = AST.MapField
	<$ tok (string "map<")
	<*> keyType
	<* tok (char ',')
	<*> type_
	<* tok (char '>')
	<*> ident
	<* tok (char '=')
	<*> fieldNumber
	<*> brackets (fieldOption `sepBy` tok (char ','))

reserved :: PP AST.Reserved
reserved = reservedRanges <|> reservedNames
	where
		reservedToken = tok (string "reserved")
		reservedRanges = AST.ReservedRanges
			<$ reservedToken
			<*> range `sepBy` tok (char ',')
			<* tok (char ';')
		reservedNames = AST.ReservedNames
			<$ reservedToken
			<*> ident `sepBy` tok (char ',')

range :: PP AST.Range
range = AST.Range
	<$> genericParser.integer
	<*> optionMaybe to
	where
		to = tok (string "to") *> intOrMax
		intOrMax = Left <$> genericParser.integer
			<|> Right <$> (AST.Max <$ tok (string "max"))

type_ :: PP AST.Type
type_ = AST.Type
	<$ C.option "" (tok (string "."))
	<*> (foldl (<>) "" <$> rest)
	where
		rest = identParser `sepBy` tok (char '.')

keyType :: PP AST.KeyType
keyType = AST.Int32 <$ tok (string "int32")
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

constant :: PP AST.Constant
constant = AST.ConstantIdent <$> fullIdent
	<|> AST.ConstantInt <$> genericParser.integer
	<|> AST.ConstantFloat <$> genericParser.float
	<|> AST.ConstantString <$> stringLiteral
	<|> AST.ConstantBool <$> boolLiteral

manyStatements :: forall a. PP a -> PP (List a)
manyStatements p = filterEmpty <$> many withEmpty
	where
		withEmpty :: PP (Either a Unit)
		withEmpty = Right <$> emptyStatement
			<|> (Left <$> p)

filterEmpty :: forall a. List (Either a Unit) -> List a
filterEmpty = concatMap toList
	where toList e = case e of
		Left x -> x:nil
		Right _ -> nil


stringLiteral :: PP AST.StringLiteral
stringLiteral = AST.StringLiteral <$> genericParser.stringLiteral

boolLiteral :: PP Boolean
boolLiteral = true <$ tok (string "true")
	<|> false <$ tok (string "false")

emptyStatement :: PP Unit
emptyStatement = void (tok (char ';'))

syntax :: PP Unit
syntax = void syntaxParser
	where
		syntaxParser = tok (string "syntax")
			<* tok (char '=')
			<* tok (string "\"proto3\"")
			<* tok (char ';')

fullIdent :: PP AST.FullIdent
fullIdent = AST.FullIdent <$> (ident `sepBy1` char '.')

ident :: PP AST.Ident
ident = AST.Ident <$> identParser

optempty :: forall a. Monoid a => PP a -> PP a
optempty = C.option mempty

parens :: forall a. PP a -> PP a
parens = between (char '(') (char ')')

braces :: forall a. PP a -> PP a
braces = between (char '{') (char '}')

brackets :: forall a. PP a -> PP a
brackets = between (char '[') (char ']')

between :: forall a b c. PP a -> PP b -> PP c -> PP c
between open close p = tok open *> p <* tok close