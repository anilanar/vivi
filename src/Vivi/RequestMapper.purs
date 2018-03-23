module Vivi.RequestMapper
where

import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State (StateT(..), get, put, runStateT)
import Data.Array as Arr
import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple)
import Language.GraphQL.AST as G
import Language.GraphQL.Types (List(..), catMaybes, (:), fromFoldable)
import Language.Protobuf.AST as P
import Prelude (Unit, bind, pure, ($), (+), (<$>), (<<<), (<>), (>>=))

type Warnings = List String
type Error = String
type ConvertT = StateT Warnings (ExceptT String Identity)

class Convert i o where
	convert :: i -> ConvertT o

runConvert :: forall a. ConvertT a -> List String -> Either String (Tuple a (List String))
runConvert p s = unwrap $ runExceptT $ runStateT p s

warn :: String -> ConvertT Unit
warn str = do
	all <- get
	put (str : all)

instance convertDocument :: Convert G.QueryDocument P.Document where
	convert (G.QueryDocument defs) = P.Document <$> traverse convert defs

instance convertDefinition :: Convert G.Definition P.Statement where
	convert (G.DefinitionOperation op) = P.StatementDefinition <$> convert op
	convert (G.DefinitionFragment _) = throwError "Fragment definitions are not supported"

instance convertOperationDefinition :: Convert G.OperationDefinition P.Definition where
	convert (G.Query (G.Node (G.Name name) variables _ _)) =
		P.DefinitionMessage <$> (mkMessage name variables)
	convert (G.Mutation (G.Node (G.Name name) variables _ _)) =
		P.DefinitionMessage <$> (mkMessage name variables)
	convert (G.AnonymousQuery selectionSet) =
		throwError "Anonymous queries are not supported."

instance convertVariable :: Convert G.Variable P.Ident where
	convert var = pure $ P.Ident (case var of G.Variable (G.Name name) -> name)

instance convertNamedType :: Convert G.NamedType P.Type where
	convert (G.NamedType (G.Name name)) = pure $ P.Type $ case name of
		"Int" -> "int32"
		"Float" -> "double"
		"String" -> "string"
		"Boolean" -> "bool"
		"ID" -> "string"
		_ -> name

mkMessage :: String -> List G.VariableDefinition -> ConvertT P.Message
mkMessage name variables = P.Message (P.Ident name) <<< fromFoldable
	<$> traverseWithIndex
		(\idx -> mkMessageBody (idx + 1))
		(Arr.fromFoldable variables)

mkMessageBody :: Int -> G.VariableDefinition -> ConvertT P.MessageBody
mkMessageBody idx (G.VariableDefinition var type' mDefaultVal) =
	convert var >>= \id -> mkType type' id (P.FieldNumber idx)

mkType :: G.Type -> (P.Ident -> P.FieldNumber -> ConvertT P.MessageBody)
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

mkOneOf :: G.NamedType -> P.Ident -> P.FieldNumber -> ConvertT P.MessageBody
mkOneOf namedType id@(P.Ident idAsString) fieldNumber =
	mkOneOfField namedType id fieldNumber >>= \f ->
		pure $ P.MessageBodyOneOf $ P.OneOf
			(P.Ident (idAsString <> "_optional"))
			(f : Nil)

mkOneOfField :: G.NamedType -> P.Ident -> P.FieldNumber -> ConvertT P.OneOfField
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
	-> ConvertT P.MessageBody
mkNormalField fn namedType id fieldNumber = (convert namedType) >>= \t ->
	pure $ P.MessageBodyNormalField $ fn t id fieldNumber Nil
