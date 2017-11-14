module Test.Vivi.RequestMapperSpec where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Language.GraphQL.Parser (queryDocument)
import Language.Protobuf.AST as P
import Language.Protobuf.Printer (print, runProtoPrinter)
import Language.Protobuf.Types (List(Nil), Syntax(..))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, discard, show, ($), (<*), (<<<), (<>))
import Test.Spec (Spec, describe, it)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)
import Text.Pretty (render)
import Vivi.RequestMapper (ConvertT, convert, runConvert)

spec :: forall r. Spec
   ( exception :: EXCEPTION
   , fs :: FS
   , console :: CONSOLE
   | r
   ) Unit
spec = describe "Vivi.RequestMapper" do
   it "should work" do
       gql <- liftEff $ readTextFile UTF8 "./test.gql"
       case runParser gql (queryDocument <* eof) of
           Right gDocument -> do
               let mPDocument = ((convert gDocument) :: ConvertT P.Document)
               generate mPDocument
           Left err -> log $ "error: " <> show err

generate :: forall e. ConvertT P.Document -> Aff
    ( console :: CONSOLE
    | e
    ) Unit
generate x = case runConvert x Nil of
    Left (err :: String) -> do
        log ("error: " <> show err)
    Right (Tuple pDocument warnings) -> do
        log $ "result: " <> rp pDocument
        log $ "warnings: " <> show warnings
    where
        rp = render <<< (runProtoPrinter {version: Syntax2}) <<< print