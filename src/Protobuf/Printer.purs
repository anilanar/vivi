module Language.Protobuf.Printer
where

import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Language.Protobuf.AST as AST
import Language.Protobuf.Types (List(..))
import Prelude (class Functor, show, ($), (&&), (<$>), (<<<), (==))
import Text.Pretty as P

instance printDocument :: Print AST.Document where
    print (AST.Document statements) = syntax
		<^> P.vcat (print <$> statements)

instance printStatement :: Print AST.Statement where
    print (AST.StatementImport import_) = print import_
    print (AST.StatementPackage package) = print package
    print (AST.StatementOption option) = print option
    print (AST.StatementDefinition definition) = print definition

instance printImport :: Print AST.Import where
	print (AST.Import specifier id) = print "import"
		<> printSpecifier specifier
		<> print id
		<> print ";"
		where
			printSpecifier Nothing = print " "
			printSpecifier (Just spec) = between (print " ") (print " ") (print spec)

instance printImportSpecifier :: Print AST.ImportSpecifier where
	print AST.Weak = print "weak"
	print AST.Public = print "public"

instance printPackage :: Print AST.Package where
	print (AST.Package id) = print "package" <+> print id <> print ";"

instance printDefinition :: Print AST.Definition where
	print (AST.DefinitionMessage msg) = print msg
	print (AST.DefinitionEnum enum) = print enum
	print (AST.DefinitionService svc) = print svc

instance printMessage :: Print AST.Message where
	print (AST.Message name statements) = printBody
		(print "message" <+> print name)
		statements

instance printMessageBody :: Print AST.MessageBody where
	print (AST.MessageBodyEnum enum) = print enum
	print (AST.MessageBodyMessage message) = print message
	print (AST.MessageBodyOption option) = print option
	print (AST.MessageBodyOneOf oneOf) = print oneOf
	print (AST.MessageBodyMapField mapField) = print mapField
	print (AST.MessageBodyReserved reserved) = print reserved
	print (AST.MessageBodyNormalField normalField) = print normalField

instance printNormalField :: Print AST.NormalField where
	print (AST.FieldNormal type_ name fieldNumber options) = print type_
		<+> printAssign name fieldNumber
		<+> printOptions options
		<> print ";"

	print (AST.FieldNormalRepeated type_ name fieldNumber options) =
		print "repeated"
		<+> print type_
		<+> printAssign name fieldNumber
		<+> printOptions options
		<> print ";"

instance printOneOf :: Print AST.OneOf where
	print (AST.OneOf name statements) = printBody firstLine statements
		where
			firstLine = print "oneof" <+> print name

instance printOneOfField :: Print AST.OneOfField where
	print (AST.OneOfField type_ name fieldNumber options) = print type_
		<+> printAssign name fieldNumber
		<+> printOptions options
		<> print ";"

instance printEnum :: Print AST.Enum where
	print (AST.Enum name statements) = printBody
		(print "enum" <+> print name)
		statements

instance printEnumBody :: Print AST.EnumBody where
	print (AST.EnumBodyOption opt) = print opt
	print (AST.EnumBodyField field) = print field

instance printEnumField :: Print AST.EnumField where
	print (AST.EnumField name id opts) = printAssign name id
		<+> printOptions opts
		<> print ";"

instance printEnumValueOption :: Print AST.EnumValueOption where
	print (AST.EnumValueOption name const) = printAssign name const

instance printService :: Print AST.Service where
	print (AST.Service name statements) = printBody
		(print "service" <+> print name)
		statements

instance printServiceBody :: Print AST.ServiceBody where
	print (AST.ServiceBodyOption opt) = print opt
	print (AST.ServiceBodyRpc rpc) = print rpc

instance printRpc :: Print AST.Rpc where
	print rpc@(AST.Rpc _ _ _ Nil) = printRpcDef rpc <> print ";"
	print rpc@(AST.Rpc _ _ _ statements) = printBody (printRpcDef rpc) statements

printRpcDef :: AST.Rpc -> P.Doc
printRpcDef (AST.Rpc name inputType outputType _) =  print "rpc"
	<+> print name
	<+> parens (print inputType)
	<+> print "returns"
	<+> parens (print outputType)

instance printRpcType :: Print AST.RpcType where
	print (AST.RpcType name) = print name
	print (AST.RpcTypeStream name) = print "stream" <+> print name

instance printOption :: Print AST.Option where
	print (AST.Option name const) = print "option"
		<+> printAssign name const
		<> print ";"

instance printOptionName :: Print AST.OptionName where
	print (AST.Predefined name) = print name
	print (AST.Customized fullIdent idents) = parens (print fullIdent)
		<> optempty ((<>) (print ".") <<< hSepBy ".") idents

instance printMapField :: Print AST.MapField where
	print (AST.MapField keyType type_ ident fieldNum options) = print "map<"
		<> print keyType
		<> print ", "
		<> print type_
		<> print ">"
		<+> printAssign ident fieldNum
		<+> printOptions options
		<> print ";"

instance printReserved :: Print AST.Reserved where
	print x = print "reserved" <+> print' x <> print ";"
		where
			print' (AST.ReservedRanges ranges) = hSepBy ", " ranges
			print' (AST.ReservedNames names) = hSepBy ", " names

instance printFieldOption :: Print AST.FieldOption where
	print (AST.FieldOption ident const) = printAssign ident const

instance printRange :: Print AST.Range where
	print (AST.Range min Nothing) = print min
	print (AST.Range min (Just max)) = print min
		<+> print "to"
		<+> printMax max
		where
			printMax :: (Either Int AST.Max) -> P.Doc
			printMax (Right x) = print "max"
			printMax (Left int) = print int

instance printFieldNumber :: Print AST.FieldNumber where
	print (AST.FieldNumber int) = print int

instance printType :: Print AST.Type where
	print (AST.Type name) = print name

instance printKeyType :: Print AST.KeyType where
	print AST.Float = print "float"
	print AST.Double = print "double"
	print AST.Int32 = print "int32"
	print AST.Int64 = print "int64"
	print AST.UInt32 = print "uint32"
	print AST.UInt64 = print "uint64"
	print AST.SInt32 = print "sint32"
	print AST.SInt64 = print "sint64"
	print AST.Fixed32 = print "fixed32"
	print AST.Fixed64 = print "fixed64"
	print AST.SFixed32 = print "sfixed32"
	print AST.SFixed64 = print "sfixed64"
	print AST.Bool = print "bool"
	print AST.String = print "string"

instance printConstant :: Print AST.Constant where
	print (AST.ConstantIdent x) = print x
	print (AST.ConstantInt x) = print x
	print (AST.ConstantFloat x) = print x
	print (AST.ConstantString x) = print x
	print (AST.ConstantBool x) = print x

instance printStringLiteral :: Print AST.StringLiteral where
	print (AST.StringLiteral str) = P.text "\"" <> print str <> P.text "\""

instance printInt :: Print Int where
	print x = P.text (show x)

instance printFloat :: Print Number where
	print x = P.text (show x)

instance printBoolean :: Print Boolean where
	print true = P.text "true"
	print false = P.text "false"

instance printString :: Print String where
	print x = P.text x

syntax :: P.Doc
syntax = P.text "syntax = \"proto3\";"

instance printFullIdent :: Print AST.FullIdent where
	print (AST.FullIdent indents) = hSepBy "." indents

instance printIdent :: Print AST.Ident where
	print (AST.Ident name) = print name

printAssign :: forall a b. Print a => Print b => a -> b -> P.Doc
printAssign x y = print x <+> print "=" <+> print y

printBody :: forall a. Print a => P.Doc -> List a -> P.Doc
printBody firstLine statements = firstLine
	<+> print "{"
	<^> printBody' statements
	<^> print "}"
	where
		printBody' Nil = vempty
		printBody' lst = indent 2 (P.vcat (print <$> lst))

printOptions :: forall a. Print a => List a -> P.Doc
printOptions Nil = empty
printOptions opts = print "["
		<> hSepBy ", " opts
		<> print "]"

hSepBy :: forall f m
	 . Foldable f
	=> Functor f
	=> Print m
	=> String
	-> f m
	-> P.Doc
hSepBy separator list = unwrap $ intercalate
	(P.Columns (P.text separator))
	(P.Columns <<< print <$> list)

indent :: Int -> P.Doc -> P.Doc
indent w doc = padding <> doc
	where
		padding = P.empty w (P.height doc)

besideWithSpace :: P.Doc -> P.Doc -> P.Doc
besideWithSpace a b | isEmpty b = a
besideWithSpace a b | isEmpty a = b
besideWithSpace a b = a <> P.text " " <> b

besideWithComma :: P.Doc -> P.Doc -> P.Doc
besideWithComma a b | isEmpty b = a
besideWithComma a b | isEmpty a = b
besideWithComma a b = a <> P.text ", " <> b

isEmpty :: P.Doc -> Boolean
isEmpty doc = P.width doc == 0 && P.height doc == 1

hempty :: P.Doc
hempty = P.empty 0 1

vempty :: P.Doc
vempty = P.empty 0 0

empty :: P.Doc
empty = hempty

braces :: P.Doc -> P.Doc
braces x = print "{"
	<^> print "}"

brackets :: P.Doc -> P.Doc
brackets x = between (print "[") (print "]") x

parens :: P.Doc -> P.Doc
parens x = between (print "(") (print ")") x

between :: P.Doc -> P.Doc -> P.Doc -> P.Doc
between open close d = open <> d <> close

optempty :: forall p. (List p -> P.Doc) -> List p -> P.Doc
optempty fn Nil = empty
optempty fn lst = fn lst

class Print p where
	print :: p -> P.Doc

infixl 5 besideWithSpace as <+>
infixl 5 besideWithComma as <++>
infixl 5 P.beside as <>
infixl 5 P.atop as <^>