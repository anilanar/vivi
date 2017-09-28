module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (many)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Main.Parser (PP, arguments, brackets, directive, directives, listType, namedType, optempty, parse, type_)
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert')
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (between, optionMaybe)
import Text.Parsing.Parser.String (class StringLike, char, eof, string)

main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  --parseTest " \n\r #comment hehe \n #kjn\n#abc  " unit whiteSpace
  --parseTest "\n" unit whiteSpace
  log gqlExample
  parseShow gqlExample3 parse
  --parseShow "location:@x(name: \"Anil\")" (optempty directives)
  --nameParserTest

gqlExample =
  "type Character {\n\
  \  foo: Foo!\n\
  \  bar: [Bar]!\n\
  \  quu: [Quu!]\n\
  \}"

gqlExample2 =
  "type Traveller {\n\
  \  id: ID!\n\
  \  createdAt: DateTime!\n\
  \  updatedAt: DateTime!\n\
  \  name: String!\n\
  \  location: Location! @relation(name: \"TravellerLocation\")\n\
  \  messages: [Message!]! @relation(name: \"MessagesFromTraveller\")\n\
  \}\n\
\\n\
  \type Message {\n\
  \  id: ID!\n\
  \  createdAt: DateTime!\n\
  \  updatedAt: DateTime!\n\
  \  text: String!\n\
  \  sentBy: Traveller!  @relation(name: \"MessagesFromTraveller\")\n\
  \}\n\
\\n\
  \type Location {\n\
  \  id: ID!\n\
  \  createdAt: DateTime!\n\
  \  updatedAt: DateTime!\n\
  \  traveller: Traveller! @relation(name: \"TravellerLocation\")\n\
  \  latitude: Float!\n\
  \  longitude: Float!\n\
  \}"

gqlExample3 =
  "enum DogCommand { SIT, DOWN, HEEL }\n\
  \type Dog implements Pet {\n\
  \  name: String!\n\
  \  nickname: String\n\
  \  barkVolume: Int\n\
  \  doesKnowCommand(dogCommand: DogCommand!): Boolean!\n\
  \  isHousetrained(atOtherHomes: Boolean): Boolean!\n\
  \  owner: Human\n\
  \}\n\
\\n\
  \interface Sentient {\n\
  \  name: String!\n\
  \}\n\
\\n\
  \interface Pet {\n\
  \  name: String!\n\
  \}\n\
\\n\
  \type Alien implements Sentient {\n\
  \  name: String!\n\
  \  homePlanet: String\n\
  \}\n\
\\n\
  \type Human implements Sentient {\n\
  \  name: String!\n\
  \}\n\
\\n\
  \enum CatCommand { JUMP }\n\
\\n\
  \type Cat implements Pet {\n\
  \  name: String!\n\
  \  nickname: String\n\
  \  doesKnowCommand(catCommand: CatCommand!): Boolean!\n\
  \  meowVolume: Int\n\
  \}\n\
\\n\
  \union CatOrDog = Cat | Dog\n\
  \union DogOrHuman = Dog | Human\n\
  \union HumanOrAlien = Human | Alien"

nameParserTest = parseShow "[[foo]!]!" foo

foo :: PP String
foo = fix \f -> (xyz <$> (string "foo" <|> brackets f) <*> optionMaybe (string "!"))
  where
    xyz a b = case b of
      Nothing -> a
      _ -> "(" <> a <> ")"

parseTest :: forall s a eff
  .  Show a
  => Eq a
  => StringLike s
  => s -> a -> Parser s a
  -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p = case runParser input (p <* eof) of
  Right actual -> do
    assert'
      ("expected: " <> show expected <> ", actual: " <> show actual)
      (expected == actual)
  Left err -> assert' ("error: " <> show err) false

parseShow :: forall s a eff
  .  Show a
  => StringLike s
  => s -> Parser s a
  -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseShow input p = case runParser input (p <* eof) of
  Right actual -> do
    logShow actual
  Left err -> assert' ("error: " <> show err) false