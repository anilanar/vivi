module Language.Protobuf.Types
    ( module Data.List
    , PP
    )
where

import Data.List (List(..), concatMap, fromFoldable, many, some, (:))
import Text.Parsing.Parser (Parser)

type PP a = Parser String a