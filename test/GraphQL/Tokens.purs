module Test.Language.GraphQL.Tokens
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Language.GraphQL.AST (Name(..))
import Language.GraphQL.Tokens (eol, nameParser, whiteSpace)
import Prelude (Unit, bind, discard, unit)
import Test.Assert (ASSERT)
import Test.Util (parseTest, parseTest')

assertWhiteSpace :: forall e. Eff
    ( console :: CONSOLE
    , assert :: ASSERT
    | e
    )
    Unit
assertWhiteSpace = do
    parseTest "" unit whiteSpace
    parseTest "\n\n\n" unit whiteSpace
    parseTest " #abc" unit whiteSpace
    parseTest ",,," unit whiteSpace
    parseTest " , ,\n,#,,,\n#\n,," unit whiteSpace

assertEol = do
    parseTest "" unit eol
    parseTest "\n" unit eol
    parseTest "\r" unit eol
    parseTest "\n\r" unit eol

assertNameParser = do
    parseTest' "1abc" nameParser
    parseTest "_" (Name "_") nameParser
    parseTest "_fooBar" (Name "_fooBar") nameParser
    parseTest "FooBar" (Name "FooBar") nameParser
    parseTest "fooBar" (Name "fooBar") nameParser

