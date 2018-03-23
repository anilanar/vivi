module Vivi.Scope
where

import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Language.GraphQL.AST as G
import Prelude (bind, pure, ($), (<$>), (<<<))
import Vivi.Error (ViviError(..))
import Vivi.Internal.TypeBucket (TypeBucket, getType)
import Vivi.Internal.Types (FieldName(..), TypeName(..))

data Message = Message TypeName (List Field)
data Field = Field FieldName G.Type (Maybe Message)

type Collect = ReaderT TypeBucket (Except ViviError)

collect :: G.QueryDocument -> TypeBucket -> Either ViviError (List Message)
collect queryDoc typeBucket = unwrap run
    where
        run = runExceptT $ runReaderT (queryDocument queryDoc) typeBucket

queryDocument :: G.QueryDocument -> Collect (List Message)
queryDocument (G.QueryDocument defs) = traverse definition defs

definition :: G.Definition -> Collect Message
definition (G.DefinitionOperation op) = operationDefinition op
definition (G.DefinitionFragment f) = throwError
    $ UnsupportedFragmentDefinition f

operationDefinition :: G.OperationDefinition -> Collect Message
operationDefinition (G.Query n) = node n
operationDefinition (G.Mutation node) =
    throwError UnsupportedMutation
operationDefinition (G.AnonymousQuery node) =
    throwError UnsupportedAnonymousQuery

node :: G.Node -> Collect Message
node (G.Node (G.Name name) _ _ ss) = selectionSet (TypeName name) ss

selectionSet :: TypeName -> G.SelectionSet -> Collect Message
selectionSet name ss = Message name <$> traverse (selection name) ss

selection :: TypeName -> G.Selection -> Collect Field
selection t (G.SelectionField f) = field t f
selection _ (G.SelectionFragmentSpread _) =
    throwError UnsupportedFragmentSpread
selection _ (G.SelectionInlineFragment _) =
    throwError UnsupportedInlineFragment

field :: TypeName -> G.Field -> Collect Field
field parentTypeName (G.Field _ (G.Name name) _ _ ss) = do
    typeBucket <- ask
    let fName = FieldName name
    case getType typeBucket parentTypeName fName of
        Left err -> throwError err
        Right fType -> case ss of
            Nil -> pure $ Field fName fType Nothing
            _ -> Field fName fType <<< Just
                <$> selectionSet (toPureType fType) ss

toPureType :: G.Type -> TypeName
toPureType = case _ of
    G.TypeNamed namedType -> fromNamedType namedType
    G.TypeList listType -> fromListType listType
    G.TypeNonNull (G.NonNullTypeNamed namedType) -> fromNamedType namedType
    (G.TypeNonNull (G.NonNullTypeList listType)) -> fromListType listType
    where
        fromListType (G.ListType t) = toPureType t
        fromNamedType (G.NamedType (G.Name name)) = TypeName name


-- queryDocument :: Scopes
-- data Scope = MessageScope Type (List ScopeSelection)
-- data ScopeSelection = ScopeSelection G.Selection

-- data
-- data ScopeSelectionSet = ScopeSelectionSet
    -- G.NamedType
    -- SelectionSet

-- 1. collect top level type definitions and
-- types of their fields, unique by name, into a map
-- 2. go through query doc, create a
-- tree of scope objects, like
    -- S.Message = S.Message S.TypeName (List S.Field)
    -- S.Field = S.Field S.FieldName S.FieldType (Maybe S.Message)
-- 3. go through S, create list of P.Message

-- 1. collectTypes :: G.SchemaDocument -> Map G.TypedName (Map FieldName G.Type)
-- 2. collectScopes :: G.QueryDocument -> List S.Message
-- 3. gen :: (List S.Message) -> (List P.Message)
    -- get1 :: S.Message -> P.Message where g1 is recursive