module Vivi.Internal.TypeCollector
where

import Data.Either (Either(..))
import Data.Foldable (foldM)
import Language.GraphQL.AST as G
import Vivi.Error (ViviError(..))
import Vivi.Internal.TypeBucket (TypeBucket, emptyBucket, insertType)
import Vivi.Internal.Types (FieldName(..), TypeName(..))

type TypeBucketResult = Either ViviError TypeBucket

collect :: G.SchemaDocument -> TypeBucketResult
collect schemaDoc = schemaDocument emptyBucket schemaDoc

schemaDocument :: TypeBucket -> G.SchemaDocument -> TypeBucketResult
schemaDocument bucket (G.SchemaDocument typeDefs) =
    foldM typeDefinition bucket typeDefs

typeDefinition :: TypeBucket -> G.TypeDefinition -> TypeBucketResult
typeDefinition bucket (G.TypeDefinitionObject objDef) =
    objectTypeDefinition objDef bucket
typeDefinition _ typeDef =
    Left (UnsupportedTypeDefinition typeDef)

objectTypeDefinition :: G.ObjectTypeDefinition -> TypeBucket -> TypeBucketResult
objectTypeDefinition (G.ObjectTypeDefinition name _ fieldDefs) bucket =
    foldM (fieldDefinition name) bucket fieldDefs

fieldDefinition :: G.Name -> TypeBucket -> G.FieldDefinition -> TypeBucketResult
fieldDefinition
    (G.Name parentTypeName)
    bucket
    (G.FieldDefinition (G.Name fieldName) _ fieldType)
    = insertType bucket (TypeName parentTypeName) (FieldName fieldName) fieldType