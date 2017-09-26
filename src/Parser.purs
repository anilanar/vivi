module Main.Parser
where

import Control.Alt ((<$), (<$>), (<|>))
import Control.Lazy (fix)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.List (many, some)
import Data.Monoid (class Monoid, mempty)
import Main.AST (List, genericParser)
import Main.AST as AST
import Main.Tokens (tok, whiteSpace)
import Main.Types (SParser)
import Prelude (bind, pure, ($), (*>), (<$>), (<*), (<*>))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (between, option, optionMaybe, optional, sepBy1, (<?>))
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (class StringLike, char, string)
import Text.Parsing.Parser.Token (alphaNum, letter, makeTokenParser, unGenLanguageDef)

type PP a = SParser AST.QueryDocument -> SParser a

parse = fix $ \p -> queryDocument p
    where
    queryDocument :: PP AST.QueryDocument
    queryDocument p = whiteSpace
        *> (AST.QueryDocument <$> some (definition p))
        <?> "query document error!"

    definition :: PP AST.Definition
    definition p = AST.DefinitionOperation <$> operationDefinition
        <|> AST.DefinitionFragment <$> fragmentDefinition
        <?> "definition error!"

    operationDefinition :: PP AST.OperationDefinition
    operationDefinition p = AST.Query <$ tok (string "query") <*> node
        <|> AST.Mutation <$ tok (string "mutation") <*> node
        <|> (AST.AnonymousQuery <$> selectionSet)
        <?> "operationDefinition error!"

    node :: PP AST.Node
    node p = AST.Node <$> AST.nameParser
        <*> optempty variableDefinitions
        <*> optempty directives
        <*> selectionSet

    variableDefinitions :: PP (List AST.VariableDefinition)
    variableDefinitions = genericParser.parens $ some variableDefinition

    variableDefinition :: PP AST.VariableDefinition
    variableDefinition = AST.VariableDefinition <$> variable
        <* tok (char ':')
        <*> fix type_
        <*> optionMaybe defaultValue

    defaultValue :: PP AST.DefaultValue
    defaultValue = tok (char '=') *> fix value

    variable :: PP AST.Variable
    variable = AST.Variable <$ tok (char '$') <*> AST.nameParser

    selectionSet :: PP AST.SelectionSet
    selectionSet p = genericParser.braces $ some $ selection p

    selection :: PP AST.Selection
    selection = AST.SelectionField <$> field p
        <|> AST.SelectionInlineFragment <$> inlineFragment
        <|> AST.SelectionFragmentSpread <$> fragmentSpread
        <?> "selection error!"

    field :: PP AST.Field
    field = AST.Field <$> option empty (pure <$> alias)
        <*> AST.nameParser
        <*> optempty arguments
        <*> optempty directives
        <*> optempty selectionSet

    alias :: PP AST.Alias
    alias = AST.nameParser <* tok (char ':')

    arguments :: PP (List AST.Argument)
    arguments = genericParser.parens $ some argument

    argument :: PP AST.Argument
    argument = AST.Argument
        <$> AST.nameParser
        <* tok (char ':')
        <*> (fix value)

    fragmentSpread :: PP AST.FragmentSpread
    fragmentSpread = AST.FragmentSpread
        <$ tok (string "...")
        <*> AST.nameParser
        <*> optempty directives

    inlineFragment :: PP AST.InlineFragment
    inlineFragment = AST.InlineFragment
        <$ tok (string "...")
        <*> optionMaybe (tok (string "on") *> typeCondition)
        <*> optempty directives
        <*> selectionSet p

    fragmentDefinition :: PP AST.FragmentDefinition
    fragmentDefinition = AST.FragmentDefinition
        <$ tok (string "fragment")
        <*> AST.nameParser
        <* tok (string "on")
        <*> typeCondition
        <*> optempty directives
        <*> selectionSet p

    typeCondition :: PP AST.TypeCondition
    typeCondition = namedType

    value :: PP AST.Value -> PP AST.Value
    value p = tok (AST.ValueVariable <$> (variable <?> "variable")
        <|> (number <?> "number")
        <|> AST.ValueNull <$ tok (string "null")
        <|> AST.ValueBoolean <$> (booleanValue <?> "booleanValue")
        <|> AST.ValueString <$> (stringValue <?> "stringValue")
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

    booleanValue :: SParser Boolean
    booleanValue = true <$ tok (string "true")
        <|> false <$ tok (string "false")

    stringValue :: SParser AST.StringValue
    stringValue = AST.StringValue <$> genericParser.stringLiteral

    listValue :: SParser AST.Value -> SParser AST.ListValue
    listValue p = AST.ListValue <$> genericParser.brackets (many $ value p)

    objectValue :: SParser AST.Value -> SParser AST.ObjectValue
    objectValue p = AST.ObjectValue <$> genericParser.braces (
        many (objectField p <?> "objectField"))

    objectField :: SParser AST.Value -> SParser AST.ObjectField
    objectField p = AST.ObjectField
        <$> AST.nameParser
        <* tok (char ':')
        <*> value p

    directives :: SParser (List AST.Directive)
    directives = some directive

    directive :: SParser AST.Directive
    directive = AST.Directive
        <$ tok (char '@')
        <*> AST.nameParser
        <*> optempty arguments

    type_ :: SParser AST.Type -> SParser AST.Type
    type_ p = AST.TypeList <$> listType p
        <|> AST.TypeNonNull <$> nonNullType p
        <|> AST.TypeNamed <$> namedType
        <?> "type_ error!"

    namedType :: SParser AST.NamedType
    namedType = AST.NamedType <$> AST.nameParser

    listType :: SParser AST.Type -> SParser AST.ListType
    listType p
        = AST.ListType <$> genericParser.brackets (type_ p)

    nonNullType :: SParser AST.Type -> SParser AST.NonNullType
    nonNullType p
        = AST.NonNullTypeNamed <$> namedType <* tok (char '!')
        <|> AST.NonNullTypeList <$> (listType p) <* tok (char '!')
        <?> "nonNullType error!"

    typeDefinition :: SParser AST.TypeDefinition
    typeDefinition = AST.TypeDefinitionObject <$> objectTypeDefinition
        <|> AST.TypeDefinitionInterface     <$> interfaceTypeDefinition
        <|> AST.TypeDefinitionUnion <$> unionTypeDefinition
        <|> AST.TypeDefinitionScalar <$> scalarTypeDefinition
        <|> AST.TypeDefinitionEnum <$> enumTypeDefinition
        <|> AST.TypeDefinitionInputObject <$> inputObjectTypeDefinition
        <|> AST.TypeDefinitionTypeExtension <$> typeExtensionDefinition
        <?> "typeDefinition error!"

    objectTypeDefinition :: SParser AST.ObjectTypeDefinition
    objectTypeDefinition = AST.ObjectTypeDefinition
        <$  tok (string "type")
        <*> AST.nameParser
        <*> optempty interfaces
        <*> fieldDefinitions

    interfaces :: SParser AST.Interfaces
    interfaces = tok (string "implements") *> some namedType

    fieldDefinitions :: SParser (List AST.FieldDefinition)
    fieldDefinitions = genericParser.braces $ some fieldDefinition

    fieldDefinition :: SParser AST.FieldDefinition
    fieldDefinition = AST.FieldDefinition
        <$> AST.nameParser
        <*> optempty argumentsDefinition
        <*  tok (char ':')
        <*> fix type_

    argumentsDefinition :: SParser AST.ArgumentsDefinition
    argumentsDefinition = genericParser.parens $ some inputValueDefinition

    interfaceTypeDefinition :: SParser AST.InterfaceTypeDefinition
    interfaceTypeDefinition = AST.InterfaceTypeDefinition
        <$  tok (string "interface")
        <*> AST.nameParser
        <*> fieldDefinitions

    unionTypeDefinition :: SParser AST.UnionTypeDefinition
    unionTypeDefinition = AST.UnionTypeDefinition
        <$  tok (string "union")
        <*> AST.nameParser
        <*  tok (char '=')
        <*> unionMembers

    unionMembers :: SParser (List AST.NamedType)
    unionMembers = namedType `sepBy1` tok (char '|')

    scalarTypeDefinition :: SParser AST.ScalarTypeDefinition
    scalarTypeDefinition = AST.ScalarTypeDefinition
        <$  tok (string "scalar")
        <*> AST.nameParser

    enumTypeDefinition :: SParser AST.EnumTypeDefinition
    enumTypeDefinition = AST.EnumTypeDefinition
        <$  tok (string "enum")
        <*> AST.nameParser
        <*> enumValueDefinitions

    enumValueDefinitions :: SParser (List AST.EnumValueDefinition)
    enumValueDefinitions = genericParser.braces $ some enumValueDefinition

    enumValueDefinition :: SParser AST.EnumValueDefinition
    enumValueDefinition = AST.EnumValueDefinition <$> AST.nameParser

    inputObjectTypeDefinition :: SParser AST.InputObjectTypeDefinition
    inputObjectTypeDefinition = AST.InputObjectTypeDefinition
        <$  tok (string "input")
        <*> AST.nameParser
        <*> inputValueDefinitions

    inputValueDefinitions :: SParser (List AST.InputValueDefinition)
    inputValueDefinitions = genericParser.braces $ some inputValueDefinition

    inputValueDefinition :: SParser AST.InputValueDefinition
    inputValueDefinition = AST.InputValueDefinition
        <$> AST.nameParser
        <*  tok (char ':')
        <*> fix type_
        <*> optionMaybe defaultValue

    typeExtensionDefinition :: SParser AST.TypeExtensionDefinition
    typeExtensionDefinition = AST.TypeExtensionDefinition
        <$  tok (string "extend")
        <*> objectTypeDefinition

    optempty :: forall a. Monoid a => SParser a -> SParser a
    optempty = option mempty