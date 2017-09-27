module Main.AST
where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Generic (class Generic)
import Data.Generic.Rep.Show (genericShow, genericShow')
import Data.List.Lazy as LL
import Data.Maybe (Maybe)
import Debug.Trace (spy)
import Prelude (class Monad, class Show, show, ($), (<$>), (<>))
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), GenTokenParser, alphaNum, letter, makeTokenParser)

type List = LL.List

newtype Name = Name String
derive instance gName :: Generic Name
derive newtype instance shName :: Show Name

newtype QueryDocument = QueryDocument (List Definition)
--derive instance gQueryDocument :: Generic QueryDocument
derive newtype instance shQueryDocument :: Show QueryDocument

data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
--derive instance gDefinition :: Generic Definition
instance shDefinition :: Show Definition where
    show (DefinitionOperation x) = show x
    show (DefinitionFragment x) = show x

newtype SchemaDocument = SchemaDocument (List TypeDefinition)
--derive instance gSchemaDocument :: Generic SchemaDocument
derive newtype instance shSchemaDocument :: Show SchemaDocument

data OperationDefinition
  = Query Node
  | Mutation Node
  | AnonymousQuery SelectionSet
--derive instance gOperationDefinition :: Generic OperationDefinition
instance shOperationDefinition :: Show OperationDefinition where
    show (Query x) = "(Query " <> show x <> ")"
    show (Mutation x) = "(Mutation " <> show x <> ")"
    show (AnonymousQuery x) = "(AnonymousQuery " <> show x <> ")"


data Node = Node Name (List VariableDefinition) (List Directive) SelectionSet
--derive instance gNode :: Generic Node
instance shNode :: Show Node where
    show (Node x y z j) = "(Node " <> show x <> ")"

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
--derive instance gVariableDefinition :: Generic VariableDefinition
--instance shVariableDefinition :: Show VariableDefinition where
--    show (VariableDefinition x y z) = "(VariableDefinition " <> show x <> " " <> show y <> " " show z <> ")"

newtype Variable = Variable Name
derive instance gVariable :: Generic Variable
derive newtype instance shVariable :: Show Variable

type SelectionSet = List Selection

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
--derive instance gSelection :: Generic Selection
instance shSelection :: Show Selection where
    show (SelectionField x) = "(SelectionField " <> show x <> ")"
    show (SelectionFragmentSpread x) = "(SelectionFragmentSpread " <> show x <> ")"
    show (SelectionInlineFragment x) = "(SelectionInlineFragment " <> show x <> ")"

data Field = Field (Maybe Alias) Name (List Argument) (List Directive) SelectionSet
--derive instance gField :: Generic Field
instance shField :: Show Field where
    show (Field x y z j k) = "(VariableDefinition "
        <> show x <> " " <> show y <> " " <> show z <> " " <> show j <> " "
        <> show k <> ")"

type Alias = Name

data Argument = Argument Name Value
--derive instance gArgument :: Generic Argument
instance shArgument :: Show Argument where
    show (Argument x y) = "(Argument " <> show x <> " " <> show y <> ")"

data FragmentSpread = FragmentSpread Name (List Directive)
--derive instance gFragmentSpread :: Generic FragmentSpread
instance shFragmentSpread :: Show FragmentSpread where
    show (FragmentSpread x y) = "(FragmentSpread "
        <> show x <> " " <> show y <> ")"

data InlineFragment = InlineFragment (Maybe TypeCondition) (List Directive) SelectionSet
--derive instance gInlineFragment :: Generic InlineFragment
instance shInlineFragment :: Show InlineFragment where
    show (InlineFragment x y z) = "(InlineFragment "
        <> show x <> " " <> show y <> " " <> show z <> ")"

data FragmentDefinition = FragmentDefinition Name TypeCondition (List Directive) SelectionSet
--derive instance gFragmentDefinition :: Generic FragmentDefinition
instance shFragmentDefinition :: Show FragmentDefinition where
    show (FragmentDefinition x y z j) = "(FragmentDefinition "
        <> show x <> " " <> show y <> " " <> show z <> " " <> show j <> ")"

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
--derive instance gValue :: Generic Value
instance shValue :: Show Value where
    show (ValueVariable x) = "(ValueVariable " <> show x <> ")"
    show (ValueInt x) = "(ValueInt " <> show x <> ")"
    show (ValueFloat x) = "(ValueFloat " <> show x <> ")"
    show (ValueBoolean x) = "(ValueBoolean " <> show x <> ")"
    show (ValueString x) = "(ValueString " <> show x <> ")"
    show (ValueEnum x) = "(ValueEnum " <> show x <> ")"
    show (ValueList x) = "(ValueList " <> show x <> ")"
    show (ValueObject x) = "(ValueObject " <> show x <> ")"
    show ValueNull = "ValueNull"

newtype StringValue = StringValue String
derive instance gStringValue :: Generic StringValue
derive newtype instance shStringValue :: Show StringValue

newtype ListValue = ListValue (List Value)
--derive instance gListValue :: Generic ListValue
derive newtype instance shListValue :: Show ListValue

newtype ObjectValue = ObjectValue (List ObjectField)
--derive instance gObjectValue :: Generic ObjectValue
derive newtype instance shObjectValue :: Show ObjectValue

data ObjectField = ObjectField Name Value
--derive instance gObjectField :: Generic ObjectField
instance shObjectField :: Show ObjectField where
    show (ObjectField x y) = "(ObjectField "
        <> show x <> " " <> show y <> ")"

type DefaultValue = Value

data Directive = Directive Name (List Argument)
--derive instance gDirective :: Generic Directive
instance shDirective :: Show Directive where
    show (Directive x y) = "(Directive "
        <> show x <> " " <> show y <> ")"

data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
derive instance gType :: Generic Type
instance shType :: Show Type where
    show (TypeNamed x) = "(TypeNamed " <> show x <> ")"
    show (TypeList x) = "(TypeList " <> show x <> ")"
    show (TypeNonNull x) = "(TypeNonNull " <> show x <> ")"

newtype NamedType = NamedType Name
derive instance gNamedType :: Generic NamedType
derive newtype instance shNamedType :: Show NamedType

newtype ListType = ListType Type
derive instance gListType :: Generic ListType
derive newtype instance shListType :: Show ListType

data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType
derive instance gNonNullType :: Generic NonNullType
instance shNonNullType :: Show NonNullType where
    show (NonNullTypeNamed x) = "(NonNullTypeNamed " <> show x <> ")"
    show (NonNullTypeList x) = "(NonNullTypeList " <> show x <> ")"

data TypeDefinition
  = TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  | TypeDefinitionTypeExtension TypeExtensionDefinition
--derive instance gTypeDefinition :: Generic TypeDefinition
instance shTyeDefinition :: Show TypeDefinition where
    show (TypeDefinitionObject x) = "(TypeDefinitionObject " <> show x <> ")"
    show (TypeDefinitionInterface  x) = "(TypeDefinitionInterface " <> show x <> ")"
    show (TypeDefinitionUnion x) = "(TypeDefinitionUnion " <> show x <> ")"
    show (TypeDefinitionScalar x) = "(TypeDefinitionScalar " <> show x <> ")"
    show (TypeDefinitionEnum x) = "(TypeDefinitionEnum " <> show x <> ")"
    show (TypeDefinitionInputObject x) = "(TypeDefinitionInputObject " <> show x <> ")"
    show (TypeDefinitionInputObject x) = "(TypeDefinitionInputObject " <> show x <> ")"
    show (TypeDefinitionTypeExtension x) = "(TypeDefinitionTypeExtension " <> show x <> ")"

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces (List FieldDefinition)
--derive instance gObjectTypeDefinition :: Generic ObjectTypeDefinition
instance shObjectTypeDefinition :: Show ObjectTypeDefinition where
    show _ = ""

type Interfaces = List NamedType

data FieldDefinition = FieldDefinition Name ArgumentsDefinition Type
--derive instance gFieldDefinition :: Generic FieldDefinition
instance shFieldDefinition :: Show FieldDefinition where
    show _ = ""

type ArgumentsDefinition = List InputValueDefinition

data InputValueDefinition = InputValueDefinition Name Type (Maybe DefaultValue)
--derive instance gInputValueDefinition :: Generic InputValueDefinition
instance shInputValueDefinition :: Show InputValueDefinition where
    show _ = ""

data InterfaceTypeDefinition = InterfaceTypeDefinition Name (List FieldDefinition)
--derive instance gInterfaceTypeDefinition :: Generic InterfaceTypeDefinition
instance shInterfaceTypeDefinition :: Show InterfaceTypeDefinition where
    show _ = ""

data UnionTypeDefinition = UnionTypeDefinition Name (List NamedType)
--derive instance gUnionTypeDefinition :: Generic UnionTypeDefinition
instance shUnionTypeDefinition :: Show UnionTypeDefinition where
  show _ = ""

newtype ScalarTypeDefinition = ScalarTypeDefinition Name
derive instance gScalarTypeDefinition :: Generic ScalarTypeDefinition
derive newtype instance shScalarTypeDefinition :: Show ScalarTypeDefinition

data EnumTypeDefinition = EnumTypeDefinition Name (List EnumValueDefinition)
--derive instance gEnumTypeDefinition :: Generic EnumTypeDefinition
instance shEnumTypeDefinition :: Show EnumTypeDefinition where
  show _ = ""

newtype EnumValueDefinition = EnumValueDefinition Name
derive instance gEnumValueDefinition :: Generic EnumValueDefinition
derive newtype instance shEnumValueDefinition :: Show EnumValueDefinition

data InputObjectTypeDefinition = InputObjectTypeDefinition Name (List InputValueDefinition)
--derive instance gInputObjectTypeDefinition :: Generic InputObjectTypeDefinition
instance shInputObjectTypeDefinition :: Show InputObjectTypeDefinition where
  show _ = ""

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
--derive instance gTypeExtensionDefinition :: Generic TypeExtensionDefinition
derive newtype instance shTypeExtensionDefinition :: Show TypeExtensionDefinition
