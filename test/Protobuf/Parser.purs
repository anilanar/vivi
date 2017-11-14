module Language.Protobuf.Parser.Test
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Language.Protobuf.Parser (document)
import Language.Protobuf.Types (Syntax(..), runProtoParser)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, (<*))
import Text.Parsing.Parser.String (eof)

parser :: forall e. Eff
    ( fs :: FS
    , exception :: EXCEPTION
    , console :: CONSOLE
    | e
    )
    Unit
parser = do
    example <- readTextFile UTF8 "./example.proto"
    case runProtoParser { version: Syntax3 } (document <* eof) example of
        Right actual ->
            log "done"
        Left err ->
            logShow err