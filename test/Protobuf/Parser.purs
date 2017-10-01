module Language.Protobuf.Parser.Test
where

import Control.Monad.Eff.Console (log, logShow)
import Data.Either (Either(..))
import Language.Protobuf.Parser (document)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude (bind, (<*))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (eof)

parser = do
    example <- readTextFile UTF8 "./example.proto"
    case runParser example (document <* eof) of
        Right actual ->
            log "done"
        Left err ->
            logShow err