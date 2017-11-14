module Test.Language.GraphQL.ParserSpec
where

import Data.Maybe (Maybe(..))
import Language.GraphQL.AST as AST
import Language.GraphQL.Parser (argument, arguments, between, directive, directives, inputValueDefinition, interfaces, optempty)
import Language.GraphQL.Tokens (tok)
import Language.GraphQL.Types (List(Nil), (:))
import Prelude (Unit, bind, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (parseTest, parseTest')
import Text.Parsing.Parser.String (char, string)

spec :: forall r. Spec r Unit
spec = describe "Language.GraphQL.Parser" do
    describe "between" do
        it "parses" do
            let parser = between (char '(') (char ')') (tok $ string "foo")
            parseTest "(foo)" "foo" parser
    describe "optempty" do
        it "parses when not empty" do
            let parser = optempty (tok $ string "foo")
            parseTest "foo" "foo" parser
        it "parses when empty" do
            let parser = optempty (tok $ string "foo")
            parseTest "" "" parser
    describe "interfaces" do
        it "parses single" do
            let expected = namedType' : Nil
            parseTest "implements foo" expected interfaces
        it "parses multiple" do
            let expected = namedType' : namedType' : Nil
            parseTest "implements foo foo" expected interfaces
    describe "directives" do
        let x = AST.Directive name' Nil
        it "parses single" do
            let expected = x : Nil
            parseTest "@foo" expected directives
        it "parses multiple" do
            let expected = x : x : Nil
            parseTest "@foo @foo" expected directives
    describe "directive" do
        it "parses without args" do
            let expected = directive'
            parseTest "@foo" expected directive
        it "parses with args" do
            let args = argument' : argument' : Nil
            let expected = AST.Directive name' args
            parseTest "@foo(foo: \"bar\", foo: \"bar\")" expected directive
        it "does not parse empty string" do
            err <- parseTest' "" directive
            err `shouldEqual` "Expected directive"
    describe "arguments" do
        it "does not parse empty parens" do
            err <- parseTest' "()" arguments
            err `shouldEqual` "Expected arguments"
        it "parses single" do
            let expected = argument' : Nil
            parseTest "(foo: \"bar\")" expected arguments
    describe "argument" do
        it "parses" do
            let expected = argument'
            parseTest "foo: \"bar\"" expected argument
    describe "inputValueDefinition" do
        it "parses without default value" do
            let expected = AST.InputValueDefinition name' (AST.TypeNamed namedType') Nothing
            parseTest "foo: foo" expected inputValueDefinition
        it "parses with with default value" do
            let
                expected = AST.InputValueDefinition
                    name'
                    (AST.TypeNamed namedType')
                    (Just value')
            parseTest "foo: foo = \"bar\"" expected inputValueDefinition
    where
        name' = AST.Name "foo"
        value' = AST.ValueString (AST.StringValue "bar")
        namedType' = AST.NamedType name'
        objectTypeDefinition' = AST.ObjectTypeDefinition
            name'
            Nil
        argument' = AST.Argument name' value'
        directive' = AST.Directive name' Nil
        inputValueDefinition' = AST.InputValueDefinition
            name'
            (AST.TypeNamed namedType')
            Nothing
        fieldDefinition' = AST.FieldDefinition
            name'
            Nil
            (AST.TypeNamed namedType')
