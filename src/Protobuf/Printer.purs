module Language.Protobuf.Printer
where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, traverse)
import Language.Protobuf.AST as AST
import Language.Protobuf.Types (List(Nil), Options, Syntax(..))
import Prelude (bind, pure, show, ($), (&&), (<$>), (<<<), (==))
import Text.Pretty as P

type ProtoPrinterT = ReaderT Options Identity
type DocT = ProtoPrinterT P.Doc

class Print p where
	print :: p -> DocT

runProtoPrinter :: forall a
	 . Options
	-> ProtoPrinterT a
	-> a
runProtoPrinter options printer = unwrap
	$ (runReaderT printer) options

syntaxVersion :: ProtoPrinterT Syntax
syntaxVersion = do
	options <- ask
	pure $ options.version

instance printDocument :: Print AST.Document where
    print (AST.Document statements) = syntax <^> (vmany statements)

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
	print (AST.FieldNormal type_ name fieldNumber options) = do
		v <- syntaxVersion
		printOptional v
			<+> print type_
			<+> printAssign name fieldNumber
			<+> printOptions options
			<> print ";"

	print (AST.FieldNormalRepeated type_ name fieldNumber options) =
		print "repeated"
		<+> print type_
		<+> printAssign name fieldNumber
		<+> printOptions options
		<> print ";"

printOptional :: Syntax -> DocT
printOptional Syntax2 = print "optional"
printOptional _ = empty

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

printRpcDef :: AST.Rpc -> DocT
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
			printMax :: (Either Int AST.Max) -> DocT
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
	print (AST.StringLiteral str) = string "\"" <> print str <> string "\""

instance printInt :: Print Int where
	print x = pure $ P.text (show x)

instance printFloat :: Print Number where
	print x = pure $ P.text (show x)

instance printBoolean :: Print Boolean where
	print true = pure $ P.text "true"
	print false = pure $ P.text "false"

instance printString :: Print String where
	print x = pure $ P.text x

syntax :: DocT
syntax = string "syntax = \"proto3\";"

instance printFullIdent :: Print AST.FullIdent where
	print (AST.FullIdent indents) = hSepBy "." indents

instance printIdent :: Print AST.Ident where
	print (AST.Ident name) = print name

printAssign :: forall a b. Print a => Print b => a -> b -> DocT
printAssign x y = print x <+> print "=" <+> print y

printBody :: forall a. Print a => DocT -> List a -> DocT
printBody firstLine statements = firstLine
	<+> print "{"
	<^> printBody' statements
	<^> print "}"
	where
		printBody' :: forall b. Print b => List b -> DocT
		printBody' Nil = vempty
		printBody' lst = indent 2 (vmany lst)

printOptions :: forall a. Print a => List a -> DocT
printOptions Nil = empty
printOptions opts = print "["
		<> hSepBy ", " opts
		<> print "]"

instance printDoc :: Print P.Doc where
	print doc = pure doc

hSepBy :: forall f a
	 . Foldable f
	=> Traversable f
	=> Print a
	=> String
	-> f a
	-> DocT
hSepBy separator list = do
	list' <- traverse print list
	pure $ unwrap $ intercalate (P.Columns (P.text separator)) (P.Columns <$> list')

indent :: Int -> DocT -> DocT
indent w doc = padding <> doc
	where
		padding = do
			doc' <- doc
			pure $ P.empty w (P.height doc')

besideWithSpace :: DocT
	-> DocT
	-> DocT
besideWithSpace a b = do
	bIsEmpty <- isEmpty b
	aIsEmpty <- isEmpty a
	if bIsEmpty then a else if aIsEmpty then b else a <> (string " ") <> b
-- besideWithSpace a b | isEmpty a = b
-- besideWithSpace a b = a <> P.text " " <> b

besideWithComma :: DocT
	-> DocT
	-> DocT
besideWithComma a b = do
	bIsEmpty <- isEmpty b
	aIsEmpty <- isEmpty a
	if bIsEmpty then a else if aIsEmpty then b else (a <> (string ", ") <> b)

isEmpty :: DocT -> ProtoPrinterT Boolean
isEmpty doc = do
	doc' <- doc
	pure (P.width doc' == 0 && P.height doc' == 1)

hempty :: DocT
hempty = pure $ P.empty 0 1

vempty :: DocT
vempty = pure $ P.empty 0 0

empty :: DocT
empty = hempty

brackets :: DocT -> DocT
brackets x = between (print "[") (print "]") x

parens :: DocT -> DocT
parens x = between (print "(") (print ")") x

between :: DocT -> DocT -> DocT -> DocT
between open close d = open <> d <> close

optempty :: forall p. (List p -> DocT) -> List p -> DocT
optempty fn Nil = empty
optempty fn lst = fn lst

vmany :: forall a. Print a => List a -> DocT
vmany lst = do
	lst' <- traverse print lst
	pure (P.vcat lst')

atop :: DocT -> DocT -> DocT
atop a b = do
	a' <- a
	b' <- b
	pure $ P.atop a' b'

beside :: DocT -> DocT -> DocT
beside a b = do
	a' <- a
	b' <- b
	pure $ P.beside a' b'

string :: String -> DocT
string s = pure $ P.text s

infixl 5 besideWithSpace as <+>
infixl 5 besideWithComma as <++>
infixl 5 beside as <>
infixl 5 atop as <^>
