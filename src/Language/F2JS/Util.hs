module Language.F2JS.Util where

data Name = Str String | Gen Int
          deriving (Eq, Show, Ord)

newtype Tag = Tag {getTag :: Int}
            deriving (Eq, Ord,Show)
