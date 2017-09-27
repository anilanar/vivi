module Main.Parser
where

import Control.Alt ((<$), (<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.List.Lazy (fromFoldable, many, some)
import Data.List.Lazy as LL
import Data.Monoid (class Monoid, mempty, (<>))
import Debug.Trace (spy)
import Main.AST as AST
import Main.Tokens (genericParser, nameParser, tok, whiteSpace)
import Main.Types (SParser)
import Prelude (class Monad, Unit, bind, discard, pure, show, unit, ($), (*), (*>), (<$>), (<*), (<*>))
import Text.Parsing.Parser.Combinators (optionMaybe, sepBy1, (<?>))
import Text.Parsing.Parser.String (char, string)

type MParser =
	{ parseQueryDocument :: SParser AST.QueryDocument
	, parseSchemaDocument :: SParser AST.SchemaDocument
	}

type PP a = SParser a

type List = LL.List

parse :: PP AST.SchemaDocument
parse = schemaDocument
--    where
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
node = fix $ \_ -> AST.Node
	<$> (nameParser <* mSpy "nice name")
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
selectionSet = fix \_ -> (braces (some selection))

selection :: PP AST.Selection
selection = fix \_ -> AST.SelectionField <$> field
	<|> AST.SelectionInlineFragment <$> inlineFragment
	<|> AST.SelectionFragmentSpread <$> fragmentSpread
	<?> "selection error!"

field :: PP AST.Field
field = fix $ \_ -> AST.Field
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

inlineFragment :: PP AST.InlineFragment
inlineFragment = fix $ \_ -> AST.InlineFragment
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
value = fix \_ -> tok $
	AST.ValueVariable <$> variable <?> "variable"
	<|> number <?> "number"
	<|> AST.ValueNull <$ tok (string "null")
	<|> (AST.ValueBoolean <$> booleanValue <?> "booleanValue")
	<|> (AST.ValueString <$> stringValue <?> "stringValue")
	<|> (AST.ValueEnum <$> nameParser <?> "name")
	<|> (AST.ValueList <$> listValue <?> "listValue")
	<|> (AST.ValueObject <$> objectValue <?> "objectValue")
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

listValue :: PP AST.ListValue
listValue = fix $ \_ -> AST.ListValue
	<$> brackets (many value)

objectValue :: PP AST.ObjectValue
objectValue = fix $ \_ -> AST.ObjectValue
	<$> braces (many (objectField <?> "objectField"))

objectField :: PP AST.ObjectField
objectField = fix $ \_ -> AST.ObjectField
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

type_ :: PP AST.Type
type_ = fix $ \_ -> AST.TypeNamed <$> (mSpy "typenamed" *> namedType)
	<|> AST.TypeNonNull <$> (mSpy "typenonnull" *> nonNullType)
	<|> AST.TypeList <$> (mSpy "typelist" *> listType)
	<?> "type_ error!"

namedType :: PP AST.NamedType
namedType = AST.NamedType <$> nameParser

listType :: PP AST.ListType
listType = fix $ \_ -> AST.ListType
	<$> brackets (mSpy "ff" *> type_)
	-- _ <- char '['
	-- mSpy "dd"
	-- result <- type_ p
	-- mSpy "ff"
	-- _ <- char ']'
	-- pure result

nonNullType :: PP AST.NonNullType
nonNullType = fix $ \_ ->
	AST.NonNullTypeNamed <$> xyz --namedType <* tok (char '!')
	<|> AST.NonNullTypeList <$> listType <* tok (char '!')
	<?> "nonNullType error!"
	where
		xyz = do
			mSpy "before namedType"
			name <- namedType
			_ <- tok (char '!')
			mSpy (show name)
			pure name



typeDefinition :: PP AST.TypeDefinition
typeDefinition =
	(AST.TypeDefinitionObject <$> objectTypeDefinition) -- *> p
	<|> (AST.TypeDefinitionInterface <$> interfaceTypeDefinition) -- *> p
	<|> (AST.TypeDefinitionUnion <$> unionTypeDefinition) -- *> p
	<|> (AST.TypeDefinitionScalar <$> scalarTypeDefinition) -- *> p
	<|> (AST.TypeDefinitionEnum <$> enumTypeDefinition) -- *> p
	<|> (AST.TypeDefinitionInputObject <$> inputObjectTypeDefinition) -- *> p
	<|> (AST.TypeDefinitionTypeExtension <$> typeExtensionDefinition) -- *> p
	<?> "typeDefinition error!"

objectTypeDefinition :: PP AST.ObjectTypeDefinition
objectTypeDefinition = do
	mSpy "before type"
	_ <- tok (string "type")
	mSpy "before name parser"
	name <- nameParser
	mSpy "after name parser"
	interfaces' <- optempty interfaces
	mSpy "after interfaces"
	fieldDefs <- fieldDefinitions
	mSpy "after field defs"
	pure (AST.ObjectTypeDefinition name interfaces' fieldDefs)

interfaces :: PP AST.Interfaces
interfaces = tok (string "implements") *> some namedType

fieldDefinitions :: PP (List AST.FieldDefinition)
fieldDefinitions = braces $ some fieldDefinition

fieldDefinition :: PP AST.FieldDefinition
fieldDefinition = do
	name <- nameParser
	args <- optempty argumentsDefinition
	_ <- tok (char ':')
	type' <- type_
	pure (AST.FieldDefinition name args type')
	-- <$> nameParser
	-- <*> optempty argumentsDefinition
	-- <* tok (char ':')
	-- <* mSpy "before type"
	-- <*> type_
	-- <* mSpy "after type"

mSpy :: String -> SParser Unit
mSpy s = do
	let _ = spy s
	pure unit

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
inputValueDefinition = do
	name <- nameParser
	_ <- tok (char ':')
	type' <- type_
	defVal <- optionMaybe defaultValue
	pure (AST.InputValueDefinition name type' defVal)

typeExtensionDefinition :: PP AST.TypeExtensionDefinition
typeExtensionDefinition = AST.TypeExtensionDefinition
	<$  tok (string "extend")
	<*> objectTypeDefinition

optempty :: forall a. Monoid a => SParser a -> SParser a
optempty = pure mempty

parens = between (char '(') (char ')')
braces = between (char '{') (char '}')
brackets = between (char '[') (char ']')
between open close p = tok open *> p <* tok close