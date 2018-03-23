module Vivi.Internal.Types
where

import Control.Monad.State (StateT)
import Data.Map (Map)
import Language.GraphQL.AST as G
import Prelude (class Eq, class Ord, class Show)

newtype TypeName = TypeName String
derive newtype instance showTypeName :: Show TypeName
derive newtype instance eqTypeName :: Eq TypeName
derive newtype instance ordTypeName :: Ord TypeName

newtype FieldName = FieldName String
derive newtype instance showFieldName :: Show FieldName
derive newtype instance eqFieldName :: Eq FieldName
derive newtype instance ordFieldName :: Ord FieldName

type OccurrenceCounts = Map G.Name Int
type MessageClosure m a = StateT OccurrenceCounts m a

newtype TypeBucket = TypeBucket (Map TypeName (Map FieldName G.Type))
derive newtype instance showTypeBucket :: Show TypeBucket