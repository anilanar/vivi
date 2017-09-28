module Language.GraphQL.Types
    ( PP
    , List
    , fromFoldable
    , many
    , some)
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

some :: forall f a. Alternative f => Lazy (f (LL.List a)) => f a -> f (LL.List a)
some = LL.some

--fromFoldable :: forall f. Foldable f => (forall a. f a -> List a)
fromFoldable :: forall a f. Foldable f => f a -> List a
fromFoldable = LL.fromFoldable