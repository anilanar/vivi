module Main.Parser
where

import Control.Alt ((<$), (<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.List (foldl, fromFoldable, many, some)
import Data.List as LL
import Data.Monoid (class Monoid, mempty)
import Main.AST as AST
import Main.Tokens (genericParser, nameParser, tok, whiteSpace)
import Main.Types (SParser)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, const, pure, ($), (*>), (<$>), (<*), (<*>))
import Text.Parsing.Parser.Combinators (option, optionMaybe, sepBy1, (<?>))
import Text.Parsing.Parser.String (char, string)

type MParser =
	{ parseQueryDocument :: SParser AST.QueryDocument
	, parseSchemaDocument :: SParser AST.SchemaDocument
	}

type PP a = SParser a

type List = LL.List

parse :: PP AST.SchemaDocument
parse = schemaDocument

queryDocument :: PP AST.QueryDocument
queryDocument = whiteSpace
	*> (AST.QueryDocument <$> some definition)
	<?> "query document error!"

schemaDocument :: PP AST.SchemaDocument
schemaDocument = do
	typeDefs <- some typeDefinition
	pure (AST.SchemaDocument typeDefs)
	-- <?> "type document error!"

definition :: PP AST.Definition
definition = AST.DefinitionOperation <$> operationDefinition
	<|> AST.DefinitionFragment <$> fragmentDefinition
	<?> "definition error!"

operationDefinition :: PP AST.OperationDefinition
operationDefinition = AST.Query
	<$ tok (string "query")
	<*> node
	<|> AST.Mutation
	<$ tok (string "mutation")
	<*> node
	<|> (AST.AnonymousQuery <$> selectionSet)
	<?> "operationDefinition error!"

node :: PP AST.Node
node = AST.Node
	<$> nameParser
	<*> optempty variableDefinitions
	<*> optempty directives
	<*> selectionSet

variableDefinitions :: PP (List AST.VariableDefinition)
variableDefinitions = parens $ some variableDefinition

variableDefinition :: PP AST.VariableDefinition
variableDefinition = AST.VariableDefinition
	<$> variable
	<* tok (char ':')
	<*> type_
	<*> optionMaybe defaultValue

defaultValue :: PP AST.DefaultValue
defaultValue = tok (char '=') *> value

variable :: PP AST.Variable
variable = AST.Variable <$ tok (char '$') <*> nameParser

selectionSet :: PP AST.SelectionSet
selectionSet = fix \rec -> (braces (some (selection rec)))

selection :: PP AST.SelectionSet -> PP AST.Selection
selection selectionSet = AST.SelectionField <$> field selectionSet
	<|> AST.SelectionInlineFragment <$> inlineFragment selectionSet
	<|> AST.SelectionFragmentSpread <$> fragmentSpread
	<?> "selection error!"

field :: PP AST.SelectionSet -> PP AST.Field
field selectionSet = AST.Field
	<$> optionMaybe alias
	<*> nameParser
	<*> optempty arguments
	<*> optempty directives
	<*> optempty selectionSet

alias :: PP AST.Alias
alias = nameParser <* tok (char ':')

arguments :: PP (List AST.Argument)
arguments = parens $ some argument

argument :: PP AST.Argument
argument = AST.Argument
	<$> nameParser
	<* tok (char ':')
	<*> value

fragmentSpread :: PP AST.FragmentSpread
fragmentSpread = AST.FragmentSpread
	<$ tok (string "...")
	<*> nameParser
	<*> optempty directives

inlineFragment :: PP AST.SelectionSet -> PP AST.InlineFragment
inlineFragment selectionSet = AST.InlineFragment
	<$ tok (string "...")
	<*> optionMaybe (tok (string "on") *> typeCondition)
	<*> optempty directives
	<*> selectionSet

fragmentDefinition :: PP AST.FragmentDefinition
fragmentDefinition = AST.FragmentDefinition
	<$ tok (string "fragment")
	<*> nameParser
	<* tok (string "on")
	<*> typeCondition
	<*> optempty directives
	<*> selectionSet

typeCondition :: PP AST.TypeCondition
typeCondition = namedType

value :: PP AST.Value
value = fix \rec -> tok $
	AST.ValueVariable <$> variable <?> "variable"
	<|> number <?> "number"
	<|> AST.ValueNull <$ tok (string "null")
	<|> (AST.ValueBoolean <$> booleanValue <?> "booleanValue")
	<|> (AST.ValueString <$> stringValue <?> "stringValue")
	<|> (AST.ValueEnum <$> nameParser <?> "name")
	<|> (AST.ValueList <$> listValue rec <?> "listValue")
	<|> (AST.ValueObject <$> objectValue rec <?> "objectValue")
	<?> "value error!"
	where
		number = do
			num <- genericParser.naturalOrFloat
			case num of
				Left n -> pure (AST.ValueInt n)
				Right f -> pure (AST.ValueFloat f)

booleanValue :: PP Boolean
booleanValue = true <$ tok (string "true")
	<|> false <$ tok (string "false")

stringValue :: PP AST.StringValue
stringValue = AST.StringValue <$> genericParser.stringLiteral

listValue :: PP AST.Value -> PP AST.ListValue
listValue value = AST.ListValue <$> brackets (many value)

objectValue :: PP AST.Value -> PP AST.ObjectValue
objectValue value = AST.ObjectValue
	<$> braces (many (objectField value <?> "objectField"))

objectField :: PP AST.Value -> PP AST.ObjectField
objectField value = AST.ObjectField
	<$> nameParser
	<* tok (char ':')
	<*> value

directives :: PP (List AST.Directive)
directives = some directive

directive :: PP AST.Directive
directive = AST.Directive
	<$ tok (char '@')
	<*> nameParser
	<*> optempty arguments
	<?> "directive"

type_ :: PP AST.Type
type_ = fix $ \rec -> foldl reduce'
	<$> prefix rec
	<*> optionMaybe postfix
	<?> "type_ error!"
	where
		prefix rec = AST.TypeList <$> listType rec <|> AST.TypeNamed <$> namedType
		postfix = const true <$> tok (char '!')
		reduce' t _ = unsafePartial $ AST.TypeNonNull case t of
			AST.TypeList r ->  AST.NonNullTypeList r
			AST.TypeNamed r -> AST.NonNullTypeNamed r

namedType :: PP AST.NamedType
namedType = AST.NamedType <$> nameParser

listType :: PP AST.Type -> PP AST.ListType
listType type_ = AST.ListType <$> brackets type_

typeDefinition :: PP AST.TypeDefinition
typeDefinition =
	(AST.TypeDefinitionObject <$> objectTypeDefinition)
	<|> (AST.TypeDefinitionInterface <$> interfaceTypeDefinition)
	<|> (AST.TypeDefinitionUnion <$> unionTypeDefinition)
	<|> (AST.TypeDefinitionScalar <$> scalarTypeDefinition)
	<|> (AST.TypeDefinitionEnum <$> enumTypeDefinition)
	<|> (AST.TypeDefinitionInputObject <$> inputObjectTypeDefinition)
	<|> (AST.TypeDefinitionTypeExtension <$> typeExtensionDefinition)
	<?> "typeDefinition error!"

objectTypeDefinition :: PP AST.ObjectTypeDefinition
objectTypeDefinition = AST.ObjectTypeDefinition
	<$ tok (string "type")
	<*> nameParser
	<*> optempty interfaces
	<*> fieldDefinitions

interfaces :: PP AST.Interfaces
interfaces = tok (string "implements") *> some namedType

fieldDefinitions :: PP (List AST.FieldDefinition)
fieldDefinitions = braces $ some fieldDefinition

fieldDefinition :: PP AST.FieldDefinition
fieldDefinition = AST.FieldDefinition
	<$> nameParser
	<*> optempty argumentsDefinition
	<* tok (char ':')
	<*> type_
	<* optempty directives

argumentsDefinition :: PP AST.ArgumentsDefinition
argumentsDefinition = parens $ some inputValueDefinition

interfaceTypeDefinition :: PP AST.InterfaceTypeDefinition
interfaceTypeDefinition = AST.InterfaceTypeDefinition
	<$  tok (string "interface")
	<*> nameParser
	<*> fieldDefinitions

unionTypeDefinition :: PP AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
	<$  tok (string "union")
	<*> nameParser
	<*  tok (char '=')
	<*> unionMembers

unionMembers :: PP (List AST.NamedType)
unionMembers = fromFoldable <$> (namedType `sepBy1` tok (char '|'))

scalarTypeDefinition :: PP AST.ScalarTypeDefinition
scalarTypeDefinition = AST.ScalarTypeDefinition
	<$  tok (string "scalar")
	<*> nameParser

enumTypeDefinition :: PP AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
	<$  tok (string "enum")
	<*> nameParser
	<*> enumValueDefinitions

enumValueDefinitions :: PP (List AST.EnumValueDefinition)
enumValueDefinitions = braces (some enumValueDefinition)

enumValueDefinition :: PP AST.EnumValueDefinition
enumValueDefinition = AST.EnumValueDefinition <$> nameParser

inputObjectTypeDefinition :: PP AST.InputObjectTypeDefinition
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
	<$  tok (string "input")
	<*> nameParser
	<*> inputValueDefinitions

inputValueDefinitions :: PP (List AST.InputValueDefinition)
inputValueDefinitions = braces (some inputValueDefinition)

inputValueDefinition :: PP AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
	<$> nameParser
	<* tok (char ':')
	<*> type_
	<*> optionMaybe defaultValue

typeExtensionDefinition :: PP AST.TypeExtensionDefinition
typeExtensionDefinition = AST.TypeExtensionDefinition
	<$  tok (string "extend")
	<*> objectTypeDefinition

optempty :: forall a. Monoid a => SParser a -> SParser a
optempty = option mempty

parens :: forall a. PP a -> PP a
parens = between (char '(') (char ')')

braces :: forall a. PP a -> PP a
braces = between (char '{') (char '}')

brackets :: forall a. PP a -> PP a
brackets = between (char '[') (char ']')

between :: forall a b c. PP a -> PP b -> PP c -> PP c
between open close p = tok open *> p <* tok close