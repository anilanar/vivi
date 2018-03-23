module Vivi.ResponseMapper
where

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Array as Arr
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(Nil), concat, fromFoldable, (:))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple)
import Language.GraphQL.AST as G
import Language.Protobuf.AST as P
import Prelude (Unit, bind, pure, ($), (+), (<$>), (<<<), (<>), (>>=))
import Vivi.Internal.TypeBucket (TypeBucket, emptyBucket)
import Vivi.Internal.TypeNameGenerator (runMessageClosure)
import Vivi.Internal.Types (TypeName(..), MessageClosure)

newtype ConvertState = ConvertState
	{ warnings :: List String
	, types :: TypeBucket
	}
type Error = String
type ConvertT m = StateT ConvertState (ExceptT String m)
type Convert = ConvertT Identity

class ConvertMonad i o where
	convert :: i -> Convert o

runConvert :: forall a. Convert a -> Either String (Tuple a ConvertState)
runConvert p = unwrap $ runExceptT $ runStateT p (ConvertState { warnings: Nil, types: emptyBucket })

warn :: String -> Convert Unit
warn str = do
	(ConvertState state) <- get
	put $ ConvertState state { warnings = str : state.warnings }

-- getTypeOf :: G.NamedType -> G.Name -> Convert G.Type
-- getTypeOf t f = do
-- 	ConvertState { types } <- get
-- 	case getType types t f of
-- 		Left t' -> pure t'
-- 		Right err -> throwError (show err)

-- putType :: TypeName -> FieldName -> TypeName -> Convert Unit
-- putType t f t' = do
-- 	(ConvertState state) <- get
-- 	case insertType state.types t f t' of
-- 		Left newTypes -> put $ ConvertState state { types = newTypes }
-- 		Right error -> throwError $ show error

foo :: G.SchemaDocument -> G.QueryDocument -> Convert P.Document
foo schemaDoc queryDoc = convert queryDoc

bar :: G.QueryDocument -> Convert P.Document
bar queryDoc = convert queryDoc

-- instance convertSchemaDocument :: Convert G.SchemaDocument (List P.Statement) where
-- 	convert (G.SchemaDocument defs) = do
-- 		maybeStatements <- traverse convert defs
-- 		pure $ catMaybes maybeStatements

instance convertQueryDocument :: ConvertMonad G.QueryDocument P.Document where
	convert (G.QueryDocument defs) = P.Document <$> traverse convert defs

instance convertDefinition :: ConvertMonad G.Definition P.Statement where
	convert (G.DefinitionOperation op) = P.StatementDefinition <$> convert op
	convert (G.DefinitionFragment _) = throwError "Fragment definitions are not supported"

instance convertOperationDefinition :: ConvertMonad G.OperationDefinition P.Definition where
	convert (G.Query (G.Node name@(G.Name name') _ _ selectionSet)) =
		P.DefinitionMessage <$> (mkMessage (TypeName name') name selectionSet)
	convert (G.Mutation _) =
		throwError "Mutation operations are not supported."
	convert (G.AnonymousQuery selectionSet) =
		throwError "Anonymous queries are not supported."

instance convertNamedType :: ConvertMonad G.NamedType P.Type where
	convert (G.NamedType (G.Name name)) = pure $ P.Type $ case name of
		"Int" -> "int32"
		"Float" -> "double"
		"String" -> "string"
		"Boolean" -> "bool"
		"ID" -> "string"
		_ -> name

type ConvertClosure a = MessageClosure (ConvertT Identity) a

mkMessage :: TypeName -> G.Name -> G.SelectionSet -> Convert P.Message
mkMessage typeName (G.Name name) selectionSet = ((P.Message (P.Ident name)) <<< concat <<< fromFoldable)
	<$> runMessageClosure closure
	where
		closure = traverseWithIndex
			(\idx -> mkMessageBodies (idx + 1) typeName)
			(Arr.fromFoldable selectionSet)

mkMessageBodies :: Int -> TypeName -> G.Selection -> ConvertClosure (List P.MessageBody)
mkMessageBodies idx typeName (G.SelectionField (G.Field _ name _ _ selectionSet)) = do
	case selectionSet of
		Nil -> do
			-- Get leaf type from Schema
			-- leafType :: ParentType -> FieldName -> m TypeName
			field <- lift $ mkField (G.TypeNamed (G.NamedType (G.Name "leafType"))) name idx
			pure $ field : Nil
		_ -> do
			-- Get new random type for current type
			-- randomType :: Type -> m TypeName
			-- randomName <- gen typeName
			msg <- lift $ mkMessage typeName (G.Name "randomName") selectionSet
			field <- lift $ mkField (G.TypeNamed (G.NamedType (G.Name "randomType"))) name idx
			pure $ P.MessageBodyMessage msg : field : Nil
mkMessageBodies _ _ _ = throwError ""

mkField :: G.Type -> G.Name -> Int -> Convert P.MessageBody
mkField type' (G.Name name) idx = mkType type' (P.Ident name) (P.FieldNumber idx)

mkType :: G.Type -> (P.Ident -> P.FieldNumber -> Convert P.MessageBody)
mkType = case _ of
	G.TypeNonNull (G.NonNullTypeNamed namedType) ->
		mkNormalField P.FieldNormal namedType
	G.TypeNonNull
		(G.NonNullTypeList (G.ListType
			(G.TypeNonNull (G.NonNullTypeNamed namedType))
		)) -> mkNormalField P.FieldNormalRepeated namedType
	G.TypeNonNull
		(G.NonNullTypeList (G.ListType
			(G.TypeNamed (namedType))
		)) -> mkNormalField P.FieldNormalRepeated namedType
	G.TypeNamed namedType -> mkOneOf namedType
	G.TypeList (G.ListType (G.TypeNamed namedType)) -> \id idx -> do
		mkNormalField P.FieldNormalRepeated namedType id idx
	_ -> (\_ _ -> throwError "Unrecognized type.")

mkOneOf :: G.NamedType -> P.Ident -> P.FieldNumber -> Convert P.MessageBody
mkOneOf namedType id@(P.Ident idAsString) fieldNumber =
	mkOneOfField namedType id fieldNumber >>= \f ->
		pure $ P.MessageBodyOneOf $ P.OneOf
			(P.Ident (idAsString <> "_optional"))
			(f : Nil)

mkOneOfField :: G.NamedType -> P.Ident -> P.FieldNumber -> Convert P.OneOfField
mkOneOfField namedType id fieldNumber = convert namedType >>= \t ->
	pure $ P.OneOfField t id fieldNumber Nil

mkNormalField :: (P.Type
	-> P.Ident
	-> P.FieldNumber
	-> List P.FieldOption
	-> P.NormalField)
	-> G.NamedType
	-> P.Ident
	-> P.FieldNumber
	-> Convert P.MessageBody
mkNormalField fn namedType id fieldNumber = (convert namedType) >>= \t ->
	pure $ P.MessageBodyNormalField $ fn t id fieldNumber Nil