module Main.AST
where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (many, some)
import Data.Char (toCharCode)
import Data.Char.Unicode (isDigit)
import Data.Identity (Identity(..))
import Data.List as LL
import Data.Maybe (Maybe)
import Data.String (fromCharArray)
import Main.Tokens (tok)
import Main.Types (SParser)
import Prelude (class Monad, ($), (&&), (<$>), (<*>), (<<<), (<=), (<>), (==), (>=), (||))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, letter, makeTokenParser, unGenLanguageDef)

type List = LL.List

newtype Name = Name String
newtype QueryDocument = QueryDocument (List Definition)
data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
newtype SchemaDocument = SchemaDocument (List TypeDefinition)
data OperationDefinition
  = Query Node
  | Mutation Node
  | AnonymousQuery Node
data Node = Node Name (List VariableDefinition) (List Directive) SelectionSet
data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
newtype Variable = Variable Name
type SelectionSet = List Selection
data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
data Field = Field (Maybe Alias) Name (List Argument) (List Directive) SelectionSet
type Alias = Name
data Argument = Argument Name Value
data FragmentSpread = FragmentSpread Name (List Directive)
data InlineFragment = InlineFragment (Maybe TypeCondition) (List Directive) SelectionSet
data FragmentDefinition = FragmentDefinition Name TypeCondition (List Directive) SelectionSet
type TypeCondition = NamedType

data Value
  = ValueVariable Variable
  | ValueInt Int
  | ValueFloat Number
  | ValueBoolean Boolean
  | ValueString StringValue
  | ValueEnum Name
  | ValueList ListValue
  | ValueObject ObjectValue
  | ValueNull

newtype StringValue = StringValue String
newtype ListValue = ListValue (List Value)
newtype ObjectValue = ObjectValue (List ObjectField)
data ObjectField = ObjectField Name Value
type DefaultValue = Value
data Directive = Directive Name (List Argument)
data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
newtype NamedType = NamedType Name
newtype ListType = ListType Type
data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType

data TypeDefinition
  = TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  | TypeDefinitionTypeExtension TypeExtensionDefinition

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces (List FieldDefinition)
type Interfaces = List NamedType
data FieldDefinition = FieldDefinition Name ArgumentsDefinition Type
type ArgumentsDefinition = List InputValueDefinition
data InputValueDefinition = InputValueDefinition Name Type (Maybe DefaultValue)
data InterfaceTypeDefinition = InterfaceTypeDefinition Name (List FieldDefinition)
data UnionTypeDefinition = UnionTypeDefinition Name (List NamedType)
newtype ScalarTypeDefinition = ScalarTypeDefinition Name
data EnumTypeDefinition = EnumTypeDefinition Name (List EnumValueDefinition)
newtype EnumValueDefinition = EnumValueDefinition Name
data InputObjectTypeDefinition = InputObjectTypeDefinition Name (List InputValueDefinition)
newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition

nameParser :: SParser Name
nameParser = Name <$> genericParser.identifier

genericParser :: forall m. Monad m => GenTokenParser String m
genericParser = makeTokenParser $ LanguageDef def
    where
        def =
            { commentStart: ""
            , commentEnd: ""
            , commentLine: "#"
            , nestedComments: false
            , identStart: letter <|> char '_'
            , identLetter: alphaNum <|> char '_'
            , opStart: empty
            , opLetter: empty
            , reservedOpNames: []
            , reservedNames: []
            , caseSensitive: true
            }