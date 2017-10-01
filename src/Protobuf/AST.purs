module Language.Protobuf.AST
where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Language.Protobuf.Types (List)

data Document = Document (List Statement)

data Statement =
	StatementImport Import
	| StatementPackage Package
	| StatementOption Option
	| StatementDefinition Definition

data Import = Import (Maybe ImportSpecifier) StringLiteral
data ImportSpecifier = Weak | Public

data Package = Package FullIdent
data Constant
	= ConstantIdent FullIdent
	| ConstantInt Int
	| ConstantFloat Number
	| ConstantString StringLiteral
	| ConstantBool Boolean

data Definition
	= DefinitionMessage Message
	| DefinitionEnum Enum
	| DefinitionService Service

data Message = Message Ident (List MessageBody)
data MessageBody
	= MessageBodyNormalField NormalField
	| MessageBodyEnum Enum
	| MessageBodyMessage Message
	| MessageBodyOption Option
	| MessageBodyOneOf OneOf
	| MessageBodyMapField MapField
	| MessageBodyReserved Reserved

data Enum = Enum Ident (List EnumBody)
data EnumBody
	= EnumBodyOption Option
	| EnumBodyField EnumField

data EnumField = EnumField Ident Int (List EnumValueOption)
data EnumValueOption = EnumValueOption OptionName Constant

data Service = Service Ident ServiceBody
data ServiceBody
	= ServiceBodyOption Option
	| ServiceBodyRpc Rpc

data NormalField
	= FieldNormal Type Ident FieldNumber (List FieldOption)
	| FieldNormalRepeated Type Ident FieldNumber (List FieldOption)

data MapField = MapField KeyType Type Ident FieldNumber (List FieldOption)
data FieldOption = FieldOption Ident Constant
data OneOf = OneOf Ident (List OneOfField)
data OneOfField = OneOfField Type Ident FieldNumber (List FieldOption)
data Reserved
	= ReservedRanges (List Range)
	| ReservedNames (List Ident)
data Range = Range Int (Maybe (Either Int Max))
data Max = Max

data Rpc = Rpc Ident RpcType RpcType
data RpcType
	= RpcType Ident
	| RpcTypeStream Ident

data KeyType
	= Int32 | Int64 | UInt32 | UInt64 | SInt32 | SInt64 | Fixed32
	| Fixed64 | SFixed32 | SFixed64 | Bool | String
newtype Type = Type String
newtype FieldNumber = FieldNumber Int

data Option = Option OptionName Constant
data OptionName
	= Predefined Ident
	| Customized FullIdent (List Ident)

data StringLiteral = StringLiteral String

data FullIdent = FullIdent (List Ident)
data Ident = Ident String