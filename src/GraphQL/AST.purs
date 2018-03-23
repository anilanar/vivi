module Language.GraphQL.AST
where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Language.GraphQL.Types (List)
import Prelude (class Eq, class Ord, class Show, show, (<>))

newtype Name = Name String
derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name
derive newtype instance showName :: Show Name

newtype QueryDocument = QueryDocument (List Definition)

data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition

newtype SchemaDocument = SchemaDocument (List TypeDefinition)

data OperationDefinition
  = Query Node
  | Mutation Node
  | AnonymousQuery SelectionSet

data Node = Node Name (List VariableDefinition) (List Directive) SelectionSet

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)

newtype Variable = Variable Name
derive instance eqVariable :: Eq Variable

type SelectionSet = List Selection

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

data Field = Field (Maybe Alias) Name (List Argument) (List Directive) SelectionSet

type Alias = Name

data Argument = Argument Name Value
derive instance eqArgument :: Eq Argument

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
derive instance eqValue :: Eq Value

newtype StringValue = StringValue String
derive instance eqStringValue :: Eq StringValue

newtype ListValue = ListValue (List Value)
derive instance eqListValue :: Eq ListValue

newtype ObjectValue = ObjectValue (List ObjectField)
derive instance eqObjectValue :: Eq ObjectValue

data ObjectField = ObjectField Name Value
derive instance eqObjectField :: Eq ObjectField

type DefaultValue = Value

data Directive = Directive Name (List Argument)
derive instance eqDirective :: Eq Directive

data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
derive instance eqType :: Eq Type
derive instance ordType :: Ord Type
instance showType :: Show Type where
  show (TypeNamed namedType) = "(" <> show namedType <> ")"
  show (TypeList listType) = "(" <> show listType <> ")"
  show (TypeNonNull nonNullType) = "(" <> show nonNullType <> ")"

newtype NamedType = NamedType Name
derive newtype instance eqNamedType :: Eq NamedType
derive instance newtypeNamedType :: Newtype NamedType _
derive newtype instance ordNamedType :: Ord NamedType
derive newtype instance showNamedType :: Show NamedType

newtype ListType = ListType Type
derive newtype instance eqListType :: Eq ListType
derive instance newtypeListType :: Newtype ListType _
derive newtype instance ordListType :: Ord ListType
derive newtype instance showListType :: Show ListType

data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType
derive instance eqNonNullType :: Eq NonNullType
derive instance ordNonNullType :: Ord NonNullType
instance showNonNullType :: Show NonNullType where
  show (NonNullTypeNamed namedType) = "(NonNull " <> show namedType <> ")"
  show (NonNullTypeList listType) = "(NonNull " <> show listType <> ")"

data TypeDefinition
  = TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  | TypeDefinitionTypeExtension TypeExtensionDefinition

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces (List FieldDefinition)
derive instance eqObjectTypeDefinition :: Eq ObjectTypeDefinition

type Interfaces = List NamedType

data FieldDefinition = FieldDefinition Name ArgumentsDefinition Type
derive instance eqFieldDefinition :: Eq FieldDefinition

type ArgumentsDefinition = List InputValueDefinition

data InputValueDefinition = InputValueDefinition Name Type (Maybe DefaultValue)
derive instance eqInputValueDefinition :: Eq InputValueDefinition

data InterfaceTypeDefinition = InterfaceTypeDefinition Name (List FieldDefinition)

data UnionTypeDefinition = UnionTypeDefinition Name (List NamedType)

newtype ScalarTypeDefinition = ScalarTypeDefinition Name

data EnumTypeDefinition = EnumTypeDefinition Name (List EnumValueDefinition)

newtype EnumValueDefinition = EnumValueDefinition Name

data InputObjectTypeDefinition = InputObjectTypeDefinition Name (List InputValueDefinition)

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
derive instance eqTypeExtensionDefinition :: Eq TypeExtensionDefinition