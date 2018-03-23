module Vivi.Internal.TypeBucket
where

import Data.Either (Either(..))
import Data.Map (empty, insert, lookup, singleton)
import Data.Maybe (Maybe(..))
import Language.GraphQL.AST (Type)
import Prelude (($), (<<<))
import Vivi.Error (ViviError(..))
import Vivi.Internal.Types (FieldName, TypeBucket(..), TypeName)

emptyBucket :: TypeBucket
emptyBucket = TypeBucket empty

getType :: TypeBucket -> TypeName -> FieldName -> Either ViviError Type
getType (TypeBucket map) t f = case lookup t map of
	Just fieldMap -> case lookup f fieldMap of
		Just res -> Right res
		Nothing -> Left $ UnknownField t f
	Nothing -> Left $ UnknownType t

insertType :: TypeBucket -> TypeName -> FieldName -> Type -> Either ViviError TypeBucket
insertType (TypeBucket map) t f t' = case lookup t map of
	Just fieldMap -> case lookup f fieldMap of
		Just _ -> Left $ DuplicateField t f
		Nothing -> Right <<< TypeBucket $ insert t (insert f t' fieldMap) map
	Nothing -> Right <<< TypeBucket $ insert t (singleton f t') map