module Language.F2JS.Util where

data PrimOp = Plus | Minus | Times | Divide | Modulo | ShL  | ShR
            | CmpEQ | CmpLT | CmpLE | CmpGT | CmpGE

data Name = Str String | Gen Int
          deriving (Eq, Show, Ord)

newtype Tag = Tag {getTag :: Int}
            deriving (Eq, Ord,Show)
