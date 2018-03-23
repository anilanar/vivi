module Vivi.Internal.TypeNameGenerator
where

import Control.Monad.State (get, put, runStateT)
import Data.Map (empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Language.GraphQL.AST as G
import Prelude (class Monad, bind, pure, show, (+), (<$>), (<>))
import Vivi.Internal.Types (MessageClosure)

gen :: forall m. Monad m => G.Name -> MessageClosure m G.Name
gen type'@(G.Name typeName) = do
    counts <- get
    case lookup type' counts of
        Nothing -> do
            _ <- put (insert type' 1 counts)
            pure (G.Name (typeName <> "0"))
        Just count -> do
            _ <- put (insert type' (count + 1) counts)
            pure (G.Name (typeName <> show (count + 5)))

runMessageClosure :: forall m a. Monad m => MessageClosure m a -> m a
runMessageClosure g = fst <$> runStateT g empty