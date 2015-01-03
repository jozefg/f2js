{-# LANGUAGE LambdaCase #-}
module Language.F2JS.CodeGen where
import Control.Applicative
import Language.F2JS.Target
import Language.F2JS.Util
import qualified Language.JavaScript.AST as J
import qualified Language.JavaScript.NonEmptyList as J

jname :: String -> J.Name
jname = either error id . J.name

jvar :: Name -> J.Name
jvar (Gen i) = jname ('_' : show i)
jvar (Str s) = jname s

atom :: Atom -> J.Expr
atom (NameAtom n) = J.ExprName (jvar n)
atom (LitAtom l) = lit l

lit :: Lit -> J.Expr
lit = J.ExprLit <$> \case
  Double d -> J.LitNumber (J.Number d)
  String s -> J.LitString (either error id $ J.jsString s)
  Bool b -> J.LitBool b
  Record r ->
    J.LitObject
    . J.ObjectLit
    . map (uncurry $ J.ObjectField . Left . jvar)
    . map (fmap atom)
    $ r
