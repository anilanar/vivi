module Language.Protobuf.Types
where

import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Data.Foldable (class Foldable)
import Data.List as LL
import Text.Parsing.Parser (Parser)

type PP a = Parser String a
type List = LL.List

many :: forall f a. Alternative f => Lazy (f (LL.List a)) => f a -> f (LL.List a)
many = LL.many

concatMap :: forall a b. (a -> List b) -> List a -> List b
concatMap = LL.concatMap

some :: forall f a. Alternative f => Lazy (f (LL.List a)) => f a -> f (LL.List a)
some = LL.some

cons :: forall a. a -> List a -> List a
cons a b = LL.Cons a b

nil :: forall a. List a
nil = LL.Nil

infixr 6 cons as :

fromFoldable :: forall a f. Foldable f => f a -> List a
fromFoldable = LL.fromFoldable