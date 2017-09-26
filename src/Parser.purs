module Main.Parser
where

import Control.Alt ((<$), (<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.List (many, some)
import Data.Monoid (class Monoid, mempty)
import Debug.Trace (spy)
import Main.AST (List, genericParser)
import Main.AST as AST
import Main.Tokens (tok)
import Main.Types (SParser)
import Prelude (class Monad, Unit, bind, discard, pure, unit, ($), (*>), (<$>), (<*), (<*>))
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy1, (<?>))
import Text.Parsing.Parser.String (char, string)

type MParser =
    { parseQueryDocument :: SParser AST.QueryDocument
    , parseSchemaDocument :: SParser AST.SchemaDocument
    }

type PP a = SParser AST.SchemaDocument -> SParser a

--mkParser :: MParser
parse :: SParser AST.SchemaDocument
parse = fix $ \p -> schemaDocument p
    where
    queryDocument :: PP AST.QueryDocument
    queryDocument p = genericParser.whiteSpace
        *> (AST.QueryDocument <$> some (definition p))
        <?> "query document error!"

    schemaDocument :: PP AST.SchemaDocument
    schemaDocument p = (AST.SchemaDocument <$> some (typeDefinition p))
        <?> "type document error!"

    definition :: PP AST.Definition
    definition p = AST.DefinitionOperation <$> operationDefinition p
        <|> AST.DefinitionFragment <$> fragmentDefinition p
        <?> "definition error!"

    operationDefinition :: PP AST.OperationDefinition
    operationDefinition p = AST.Query
        <$ tok (string "query")
        <*> node p
        <|> AST.Mutation
        <$ tok (string "mutation")
        <*> node p
        <|> (AST.AnonymousQuery <$> (selectionSet p))
        <?> "operationDefinition error!"

    node :: PP AST.Node
    node p = AST.Node <$> AST.nameParser
        <*> optempty (variableDefinitions p)
        <*> optempty (directives p)
        <*> selectionSet p

    variableDefinitions :: PP (List AST.VariableDefinition)
    variableDefinitions p = genericParser.parens $ some $ variableDefinition p

    variableDefinition :: PP AST.VariableDefinition
    variableDefinition p = AST.VariableDefinition <$> variable p
        <* tok (char ':')
        <*> type_ p
        <*> optionMaybe (defaultValue p)

    defaultValue :: PP AST.DefaultValue
    defaultValue p = tok (char '=') *> value p

    variable :: PP AST.Variable
    variable _ = AST.Variable <$ tok (char '$') <*> AST.nameParser

    selectionSet :: PP AST.SelectionSet
    selectionSet p = genericParser.braces $ some $ selection p

    selection :: PP AST.Selection
    selection p = AST.SelectionField <$> field p
        <|> AST.SelectionInlineFragment <$> inlineFragment p
        <|> AST.SelectionFragmentSpread <$> fragmentSpread p
        <?> "selection error!"

    field :: PP AST.Field
    field p = AST.Field
        <$> optionMaybe (alias p)
        <*> AST.nameParser
        <*> optempty (arguments p)
        <*> optempty (directives p)
        <*> optempty (selectionSet p)

    alias :: PP AST.Alias
    alias _ = AST.nameParser <* tok (char ':')

    arguments :: PP (List AST.Argument)
    arguments p = genericParser.parens $ some $ argument p

    argument :: PP AST.Argument
    argument p = AST.Argument
        <$> AST.nameParser
        <* tok (char ':')
        <*> value p

    fragmentSpread :: PP AST.FragmentSpread
    fragmentSpread p = AST.FragmentSpread
        <$ tok (string "...")
        <*> AST.nameParser
        <*> optempty (directives p)

    inlineFragment :: PP AST.InlineFragment
    inlineFragment p = AST.InlineFragment
        <$ tok (string "...")
        <*> optionMaybe (tok (string "on") *> typeCondition p)
        <*> optempty (directives p)
        <*> selectionSet p

    fragmentDefinition :: PP AST.FragmentDefinition
    fragmentDefinition p = AST.FragmentDefinition
        <$ tok (string "fragment")
        <*> AST.nameParser
        <* tok (string "on")
        <*> typeCondition p
        <*> optempty (directives p)
        <*> selectionSet p

    typeCondition :: PP AST.TypeCondition
    typeCondition = namedType

    value :: PP AST.Value
    value p = tok (AST.ValueVariable <$> (variable p <?> "variable")
        <|> (number <?> "number")
        <|> AST.ValueNull <$ tok (string "null")
        <|> AST.ValueBoolean <$> (booleanValue p <?> "booleanValue")
        <|> AST.ValueString <$> (stringValue p <?> "stringValue")
        <|> AST.ValueEnum <$> (AST.nameParser <?> "name")
        <|> AST.ValueList <$> (listValue p <?> "listValue")
        <|> AST.ValueObject <$> (objectValue p <?> "objectValue")
        <?> "value error!")
        where
            number = do
                num <- genericParser.naturalOrFloat
                case num of
                    Left n -> pure (AST.ValueInt n)
                    Right f -> pure (AST.ValueFloat f)

    booleanValue :: PP Boolean
    booleanValue _ = true <$ tok (string "true")
        <|> false <$ tok (string "false")

    stringValue :: PP AST.StringValue
    stringValue _ = AST.StringValue <$> genericParser.stringLiteral

    listValue :: PP AST.ListValue
    listValue p = AST.ListValue <$> genericParser.brackets (many $ value p)

    objectValue :: PP AST.ObjectValue
    objectValue p = AST.ObjectValue <$> genericParser.braces (
        many (objectField p <?> "objectField"))

    objectField :: PP AST.ObjectField
    objectField p = AST.ObjectField
        <$> AST.nameParser
        <* tok (char ':')
        <*> value p

    directives :: PP (List AST.Directive)
    directives p = some $ directive p

    directive :: PP AST.Directive
    directive p = AST.Directive
        <$ tok (char '@')
        <*> AST.nameParser
        <*> optempty (arguments p)

    type_ :: PP AST.Type
    type_ p = AST.TypeNamed <$> (mSpy "ee" *> namedType p)
        <|> AST.TypeNonNull <$> (mSpy "cc" *> nonNullType p)
        <|> AST.TypeList <$> (mSpy "bb" *> listType p)
        <?> "type_ error!"

    namedType :: PP AST.NamedType
    namedType _ = AST.NamedType <$> AST.nameParser

    listType :: PP AST.ListType
    listType p = AST.ListType <$> do --genericParser.brackets (mSpy "ff" *> type_ p)
        _ <- char '['
        mSpy "dd"
        result <- type_ p
        mSpy "ff"
        _ <- char ']'
        pure result

    nonNullType :: PP AST.NonNullType
    nonNullType p
        = AST.NonNullTypeNamed <$> namedType p <* tok (char '!')
        <|> AST.NonNullTypeList <$> (listType p) <* tok (char '!')
        <?> "nonNullType error!"

    typeDefinition :: PP AST.TypeDefinition
    typeDefinition p = AST.TypeDefinitionObject <$> objectTypeDefinition p
        <|> AST.TypeDefinitionInterface     <$> interfaceTypeDefinition p
        <|> AST.TypeDefinitionUnion <$> unionTypeDefinition p
        <|> AST.TypeDefinitionScalar <$> scalarTypeDefinition p
        <|> AST.TypeDefinitionEnum <$> enumTypeDefinition p
        <|> AST.TypeDefinitionInputObject <$> inputObjectTypeDefinition p
        <|> AST.TypeDefinitionTypeExtension <$> typeExtensionDefinition p
        <?> "typeDefinition error!"

    objectTypeDefinition :: PP AST.ObjectTypeDefinition
    objectTypeDefinition p = AST.ObjectTypeDefinition
        <$  tok (string "type")
        <*> AST.nameParser
        <*> optempty (interfaces p)
        <*> fieldDefinitions p

    interfaces :: PP AST.Interfaces
    interfaces p = tok (string "implements") *> some (namedType p)

    fieldDefinitions :: PP (List AST.FieldDefinition)
    fieldDefinitions p = genericParser.braces $ some $ fieldDefinition p

    fieldDefinition :: PP AST.FieldDefinition
    fieldDefinition p = do
        name <- AST.nameParser
        args <- optempty (argumentsDefinition p)
        _ <- tok (char ':')
        mSpy "before type"
        _type <- type_ p
        mSpy "after type"
        pure $ AST.FieldDefinition name args _type

    mSpy :: String -> SParser Unit
    mSpy s = do
        let _ = spy s
        pure unit

    argumentsDefinition :: PP AST.ArgumentsDefinition
    argumentsDefinition p = genericParser.parens
        $ some (inputValueDefinition p)

    interfaceTypeDefinition :: PP AST.InterfaceTypeDefinition
    interfaceTypeDefinition p = AST.InterfaceTypeDefinition
        <$  tok (string "interface")
        <*> AST.nameParser
        <*> fieldDefinitions p

    unionTypeDefinition :: PP AST.UnionTypeDefinition
    unionTypeDefinition p = AST.UnionTypeDefinition
        <$  tok (string "union")
        <*> AST.nameParser
        <*  tok (char '=')
        <*> unionMembers p

    unionMembers :: PP (List AST.NamedType)
    unionMembers p = namedType p `sepBy1` tok (char '|')

    scalarTypeDefinition :: PP AST.ScalarTypeDefinition
    scalarTypeDefinition _ = AST.ScalarTypeDefinition
        <$  tok (string "scalar")
        <*> AST.nameParser

    enumTypeDefinition :: PP AST.EnumTypeDefinition
    enumTypeDefinition p = AST.EnumTypeDefinition
        <$  tok (string "enum")
        <*> AST.nameParser
        <*> enumValueDefinitions p

    enumValueDefinitions :: PP (List AST.EnumValueDefinition)
    enumValueDefinitions p = genericParser.braces $ some $ enumValueDefinition p

    enumValueDefinition :: PP AST.EnumValueDefinition
    enumValueDefinition _ = AST.EnumValueDefinition <$> AST.nameParser

    inputObjectTypeDefinition :: PP AST.InputObjectTypeDefinition
    inputObjectTypeDefinition p = AST.InputObjectTypeDefinition
        <$  tok (string "input")
        <*> AST.nameParser
        <*> inputValueDefinitions p

    inputValueDefinitions :: PP (List AST.InputValueDefinition)
    inputValueDefinitions p = genericParser.braces
        $ some
        $ inputValueDefinition p

    inputValueDefinition :: PP AST.InputValueDefinition
    inputValueDefinition p = AST.InputValueDefinition
        <$> AST.nameParser
        <*  tok (char ':')
        <*> type_ p
        <*> optionMaybe (defaultValue p)

    typeExtensionDefinition :: PP AST.TypeExtensionDefinition
    typeExtensionDefinition p = AST.TypeExtensionDefinition
        <$  tok (string "extend")
        <*> objectTypeDefinition p

    optempty :: forall a. Monoid a => SParser a -> SParser a
    optempty = pure mempty