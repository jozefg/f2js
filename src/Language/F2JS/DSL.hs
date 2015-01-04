module Language.F2JS.DSL where
import Data.String
import Language.F2JS.AST
import Language.F2JS.Util

instance IsString Name where
  fromString = Str
instance IsString Expr where
  fromString = Global . fromString
instance IsString Lit where
  fromString = String
instance Num Lit where
  fromInteger = Double . fromInteger
  (+) = undefined
  (*) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined
instance Num Expr where
  fromInteger = Var . fromInteger
  l + r = (PrimOp Plus `App` l) `App` r
  l * r = (PrimOp Times `App` l) `App` r
  l - r = (PrimOp Minus `App` l) `App` r
  abs = undefined
  signum = undefined
