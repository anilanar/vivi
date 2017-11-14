module Test.Language.GraphQL.TokensSpec
where

import Language.GraphQL.AST (Name(..))
import Language.GraphQL.Tokens (eol, nameParser, whiteSpace)
import Prelude (Unit, discard, unit, void, ($), (<*))
import Test.Spec (Spec, describe, it)
import Test.Util (parseTest, parseTest')

spec :: forall r. Spec r Unit
spec = describe "Language.GraphQL.Tokens" do
    describe "whiteSpace" do
        it "parses empty" do
            parseTest "" unit whiteSpace
        it "parses line endings" do
            parseTest "\n\n\n" unit whiteSpace
        it "parses space prefixed comment block" do
            parseTest " #abc" unit whiteSpace
        it "parses consecutive commas" do
            parseTest ",,," unit whiteSpace
        it "parses comma/space/eol/comment mixture" do
            parseTest " , ,\n,#,,,\n#\n,," unit whiteSpace
    describe "eol" do
        it "parses empty string" do
            parseTest "" unit eol
        it "parses single eol" do
            parseTest "\n" unit eol
        it "parses single linebreak" do
            parseTest "\r" unit eol
        it "parses lf+cr" do
            parseTest "\n\r" unit (eol <* eol)
        it "parses cr+lf" do
            parseTest "\r\n" unit (eol <* eol)
    describe "nameParser" do
        it "does not parse if first char is numeric" do
            void $ parseTest' "1abc" nameParser
        it "parses underscore" do
            parseTest "_" (Name "_") nameParser
        it "parses if first char is underscore" do
            parseTest "_fooBar" (Name "_fooBar") nameParser
        it "parses underscore in middle" do
            parseTest "foo_bar" (Name "foo_bar") nameParser
        it "parses Pascal casing" do
            parseTest "FooBar" (Name "FooBar") nameParser
        it "parses snake casing" do
            parseTest "fooBar" (Name "fooBar") nameParser
