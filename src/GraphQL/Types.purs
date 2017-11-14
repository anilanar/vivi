module Language.GraphQL.Types
    ( module Data.List
    , PP
    )
where

import Data.List (List(..), many, some, fromFoldable, catMaybes, (:))
import Text.Parsing.Parser (Parser)

type PP a = Parser String a