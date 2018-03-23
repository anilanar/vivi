module Vivi.Error
where

import Language.GraphQL.AST as G
import Prelude (class Show, show, (<>))
import Vivi.Internal.Types (FieldName, TypeName)

data ViviError
    = DuplicateField TypeName FieldName
    | UnknownType TypeName
    | UnknownField TypeName FieldName
    | UnsupportedTypeDefinition G.TypeDefinition
    | UnsupportedFragmentDefinition G.FragmentDefinition
    | UnsupportedAnonymousQuery
    | UnsupportedMutation
    | UnsupportedFragmentSpread
    | UnsupportedInlineFragment

instance showViviError :: Show ViviError where
    show (DuplicateField t f) = "Field \"" <> show f
        <> "\" is duplicate in type \"" <> show t <> "\"."
    show (UnknownType t) = "Type \"" <> show t <> "\" is unknown."
    show (UnknownField t f) = "Field \"" <> show f
        <> "\" could not be found in type \"" <> show t <> "\"."
    show (UnsupportedTypeDefinition _) = "Unsupported type definition: "
        <> "Only object type definitions are supported."
    show (UnsupportedFragmentDefinition f) = "Fragment definitions are not "
        <> "supported. Fragment definition is found with name \""
        <> name
        <> "\"."
        where
            name = case f of
                (G.FragmentDefinition (G.Name n) _ _ _) -> n
    show UnsupportedAnonymousQuery = "Anonymous queries are not supported."
    show UnsupportedMutation = "Mutation operations are not supported."
    show UnsupportedFragmentSpread = "Fragment spreads are not supported."
    show UnsupportedInlineFragment = "Inline fragments are not supported."