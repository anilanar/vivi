module Test.Language.Protobuf.PrinterSpec where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Language.Protobuf.AST as AST
import Language.Protobuf.Printer (class Print, DocT, ProtoPrinterT, atop, empty, indent, isEmpty, print, runProtoPrinter, string, syntax)
import Language.Protobuf.Types (List(..), Syntax(..), (:))
import Prelude (Unit, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Pretty (render)

spec :: forall r. Spec r Unit
spec = describe "Language.Protobuf.Printer" do
	describe "indent" do
		it "indents single line" do
			let doc1 = indent 2 $ string "Foo"
			rp' doc1 $$ "  Foo"
		it "indents stacked docs" do
			let doc1 = string "Foo"
			let doc2 = string "Bar"
			let stacked = doc1 `atop` indent 2 doc2
			let result = indent 2 stacked
			rp' result $$ "  Foo\n    Bar"
	describe "isEmpty" do
		it "says true for isEmpty empty" do
			let actual = isEmpty empty
			run actual $$ true
	describe "Ident" do
		it "prints" do
			let ident = AST.Ident "foo"
			rp ident $$ "foo"
	describe "FullIdent" do
		it "prints with single ident" do
			let ident = AST.FullIdent (AST.Ident "foo" : Nil)
			rp ident $$ "foo"
		it "prints multiple idents" do
			let identList = AST.Ident "foo" : AST.Ident "bar" : Nil
			let ident = AST.FullIdent identList
			rp ident $$ "foo.bar"
	describe "syntax" do
		it "prints" do
			rp' syntax $$ "syntax = \"proto3\";"
	describe "Int" do
		it "prints" do
			rp 5 $$ "5"
	describe "Number" do
		it "prints" do
			rp 5.4 $$ "5.4"
	describe "Boolean" do
		it "prints" do
			rp true $$ "true"
			rp false $$ "false"
	describe "StringLiteral" do
		it "prints" do
			rp (AST.StringLiteral "foo") $$ "\"foo\""
	describe "KeyType" do
		it "prints" do
			rp AST.Int32 $$ "int32"
			rp AST.Bool $$ "bool"
	describe "Type" do
		it "prints" do
			rp (AST.Type "foo.bar") $$ "foo.bar"
	describe "Range" do
		it "prints without `to`" do
			let range = AST.Range 1 Nothing
			rp range $$ "1"
		it "prints with int `to`" do
			let range = AST.Range 1 (Just (Left 5))
			rp range $$ "1 to 5"
		it "prints with max `to`" do
			let range = AST.Range 1 (Just (Right AST.Max))
			rp range $$ "1 to max"
	describe "Reserved" do
		describe "ranges" do
			let range0 = AST.Range 1 Nothing
			let range1 = AST.Range 3 (Just (Right AST.Max))

			it "prints single" do
				let reserved = AST.ReservedRanges (range0 : Nil)
				rp reserved $$ "reserved 1;"
			it "prints single" do
				let reserved = AST.ReservedRanges (range1 : Nil)
				rp reserved $$ "reserved 3 to max;"
			it "prints multiple" do
				let reserved = AST.ReservedRanges (range0 : range1 : Nil)
				rp reserved $$ "reserved 1, 3 to max;"
			it "prints multiple" do
				let reserved = AST.ReservedRanges (range1 : range0 : Nil)
				rp reserved $$ "reserved 3 to max, 1;"
		describe "names" do
			let name0 = AST.Ident "foo"
			let name1 = AST.Ident "bar"

			it "prints single" do
				let reserved = AST.ReservedNames (name0 : Nil)
				rp reserved $$ "reserved foo;"
			it "prints multiple" do
				let reserved = AST.ReservedNames (name0 : name1 : Nil)
				rp reserved $$ "reserved foo, bar;"
	describe "FieldOption" do
		it "prints" do
			let
				fieldOption = AST.FieldOption
					(AST.Ident "foo")
					(AST.ConstantInt 5)
			rp fieldOption $$ "foo = 5"
	describe "MapField" do
		let
			fieldOption0 = mkFieldOption "foo" "foo_val"
			fieldOption1 = mkFieldOption "bar" "bar_val"
			mapField = AST.MapField
				AST.Int32
				type'
				(AST.Ident "foo")
				(AST.FieldNumber 3)
			mapField0 = mapField Nil
			mapField1 = mapField (fieldOption0 : Nil)
			mapField2 = mapField (fieldOption0 : fieldOption1 : Nil)

		it "prints without field options" do
			rp mapField0 $$ "map<int32, footype> foo = 3;"
		it "prints with field options" do
			rp mapField1 $$ "map<int32, footype> foo = 3 [foo = \"foo_val\"];"
		it "prints with field options" do
			rp mapField2 $$ "map<int32, footype> foo = 3 [foo = \"foo_val\", bar = \"bar_val\"];"
	describe "OptionName" do
		let
			x = optionName'
			y = optionName''
		it "prints predefined" do
			rp x $$ "packed"
		it "prints custom" do
			rp y $$ "(foo.bar).foo"
	describe "Option" do
		let x = option'
		it "prints" do
			rp x $$ "option packed = true;"
	describe "RpcType" do
		let
			x = AST.RpcType ident'
			y = AST.RpcTypeStream ident'
		it "prints non-stream" do
			rp x $$ "foo"
		it "prints streaming" do
			rp y $$ "stream foo"
	describe "Rpc" do
		let
			x = AST.Rpc ident' (AST.RpcType ident') (AST.RpcTypeStream ident'') Nil
			y = AST.Rpc ident' (AST.RpcType ident') (AST.RpcTypeStream ident'') (option' : Nil)
		it "prints with no options" do
			rp x $$ "rpc foo (foo) returns (stream bar);"
		it "prints with options" do
			rp y $$ "rpc foo (foo) returns (stream bar) {\n  option packed = true;\n}"
	describe "ServiceBody" do
		it "prints" do
			rp (AST.ServiceBodyOption option') $$ "option packed = true;"
	describe "Service" do
		let
			x = service'
			y = AST.Service ident' (serviceBody' : Nil)
		it "prints empty" do
			rp x $$ "service foo {\n}"
		it "prints with statements" do
			rp y $$ "service foo {\n  option packed = true;\n}"
	describe "EnumValueOption" do
		let
			x = enumValueOption'
			y = AST.EnumValueOption
				(AST.Customized fullIdent' Nil)
				(AST.ConstantInt 3)
			z = AST.EnumValueOption
				(AST.Customized fullIdent' (ident' : Nil))
				(AST.ConstantInt 3)
		it "prints predefined" do
			rp x $$ "packed = true"
		it "prints custom" do
			rp y $$ "(foo.bar) = 3"
		it "prints custom" do
			rp z $$ "(foo.bar).foo = 3"
	describe "EnumField" do
		let
			x = enumField'
			y = AST.EnumField ident' 3 (enumValueOption' : Nil)
		it "prints without options" do
			rp x $$ "foo = 3;"
		it "prints with options" do
			rp y $$ "foo = 3 [packed = true];"
	describe "EnumBody" do
		let
			x = AST.EnumBodyOption option'
			y = AST.EnumBodyField enumField'
		it "prints enum option" do
			rp x $$ "option packed = true;"
		it "prints enum field" do
			rp y $$ "foo = 3;"
	describe "Enum" do
		let
			opt = AST.EnumBodyOption option'
			field = AST.EnumBodyField enumField'
			x = AST.Enum ident' (opt : field : Nil)
			y = AST.Enum ident' Nil
		it "prints non-empty" do
			let expected = "enum foo {\n  option packed = true;\n  foo = 3;\n}"
			rp x $$ expected
		it "prints empty" do
			let expected = "enum foo {\n}"
			rp y $$ expected
	describe "OneOfField" do
		let
			fieldOption = mkFieldOption "bar" "barval"
			x = AST.OneOfField
				type'
				ident'
				(AST.FieldNumber 3)
				(fieldOption : Nil)
			y = oneOfField'
		it "prints with options" do
			rp x $$ "footype foo = 3 [bar = \"barval\"];"
		it "prints without options" do
			rp y $$ "footype foo = 3;"
	describe "OneOf" do
		let
			x = AST.OneOf ident'' (oneOfField' : Nil)
			y = AST.OneOf ident'' Nil
		it "prints with statements" do
			rp x $$ "oneof bar {\n  footype foo = 3;\n}"
		it "prints with no statements" do
			rp y $$ "oneof bar {\n}"
	describe "NormalField" do
		let
			fieldOption = mkFieldOption "baropt" "bar"
			mkX = AST.FieldNormal
				type'
				ident'
				(AST.FieldNumber 3)
			mkY = AST.FieldNormalRepeated
				type'
				ident'
				(AST.FieldNumber 3)
			x = mkX Nil
			x' = mkX (fieldOption : Nil)
			y = mkY Nil
			y' = mkY (fieldOption : Nil)
		it "prints non-repeated field" do
			rp x $$ "footype foo = 3;"
		it "prints repeated field" do
			rp y $$ "repeated footype foo = 3;"
		it "prints non-repeated field with options" do
			rp x' $$ "footype foo = 3 [baropt = \"bar\"];"
		it "prints repeated field with options" do
			rp y' $$ "repeated footype foo = 3 [baropt = \"bar\"];"
	describe "MessageBody" do
		it "prints message body" do
			rp messageBody' $$ "option packed = true;"
	describe "Message" do
		let
			mkX = AST.Message ident'
			x = message'
			y = mkX (messageBody' : Nil)
			z = mkX (messageBody' : messageBody' : Nil)
		it "prints empty message" do
			rp x $$ "message foo {\n}"
		it "prints message with single statement" do
			rp y $$ "message foo {\n  option packed = true;\n}"
		it "prints message with multiple statements" do
			rp z $$ "message foo {\n  option packed = true;\n  option packed = true;\n}"
	describe "Definition" do
		let
			msg = AST.DefinitionMessage message'
			enum = AST.DefinitionEnum enum'
			service = AST.DefinitionService service'

		it "prints message definition" do
			rp msg $$ "message foo {\n}"
		it "prints enum definition" do
			rp enum $$ "enum foo {\n}"
		it "prints service definition" do
			rp service $$ "service foo {\n}"
	describe "Package" do
		it "prints" do
			rp (AST.Package fullIdent') $$ "package foo.bar;"
	describe "ImportSpecifier" do
		it "prints weak" do
			rp AST.Weak $$ "weak"
		it "prints public" do
			rp AST.Public $$ "public"
	describe "Import" do
		let
			x = AST.Import Nothing (AST.StringLiteral "foo")
			y = AST.Import (Just AST.Weak) (AST.StringLiteral "foo")
		it "prints without specifier" do
			rp x $$ "import \"foo\";"
		it "prints with specifier" do
			rp y $$ "import weak \"foo\";"
	describe "Statement" do
		it "prints import" do
			rp importS $$ "import \"foo\";"
		it "prints package" do
			rp packageS $$ "package foo.bar;"
		it "prints option" do
			rp optionS $$ "option packed = true;"
		it "prints definition" do
			rp defS $$ "message foo {\n}"
	describe "Document" do
		let
			empty = AST.Document Nil
			complex = AST.Document (packageS : importS : optionS : defS : Nil)
		it "prints empty" do
			rp empty $$ "syntax = \"proto3\";"
			rp complex $$ "syntax = \"proto3\";\npackage foo.bar;\nimport \"foo\";\noption packed = true;\nmessage foo {\n}"
	where
		ident' = AST.Ident "foo"
		ident'' = AST.Ident "bar"
		type' = AST.Type "footype"
		fullIdent' = AST.FullIdent (ident' : ident'' : Nil)
		optionName' = AST.Predefined (AST.Ident "packed")
		optionName'' = AST.Customized fullIdent' (ident' : Nil)
		option' = AST.Option
			optionName'
			(AST.ConstantBool true)
		enumValueOption' = AST.EnumValueOption
			optionName'
			(AST.ConstantBool true)
		enumField' = AST.EnumField ident' 3 Nil
		oneOfField' = AST.OneOfField
			type'
			ident'
			(AST.FieldNumber 3)
			Nil
		messageBody' = AST.MessageBodyOption option'
		message' = AST.Message ident' Nil
		enum' = AST.Enum ident' Nil
		serviceBody' = AST.ServiceBodyOption option'
		service' = AST.Service ident' Nil
		importS = AST.StatementImport $ AST.Import Nothing (AST.StringLiteral "foo")
		packageS = AST.StatementPackage $ AST.Package fullIdent'
		optionS = AST.StatementOption option'
		defS = AST.StatementDefinition $ AST.DefinitionMessage message'

mkFieldOption :: String -> String -> AST.FieldOption
mkFieldOption name val = AST.FieldOption
	(AST.Ident name)
	(AST.ConstantString (AST.StringLiteral val))

run :: forall a. ProtoPrinterT a -> a
run x = runProtoPrinter {version: Syntax3} x

rp :: forall a. Print a => a -> String
rp x = render $ runProtoPrinter {version: Syntax3} (print x)

rp' :: DocT -> String
rp' x = render $ runProtoPrinter {version: Syntax3} x

infixl 4 shouldEqual as $$