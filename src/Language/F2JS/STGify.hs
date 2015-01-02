{-# LANGUAGE LambdaCase #-}
module Language.F2JS.STGify where
import Control.Monad.Gen
import Language.F2JS.Util
import qualified Language.F2JS.AST    as A
import qualified Language.F2JS.Target as S

lit2slit :: A.Lit -> S.Lit
lit2slit = \case
  A.String s -> S.String s
  A.Double d -> S.Double d
  A.Bool b   -> S.Bool b

expr2atom :: [Name] -> A.Expr -> S.Atom
expr2atom _ (A.Lit l) = S.LitAtom (lit2slit l)
expr2atom _ (A.Global n) = S.NameAtom n
expr2atom ns (A.Var i) = S.NameAtom (ns !! i)
expr2atom _ _ = error "expr2atom failure: Tried to convert non-atom"

expr2sexpr :: [Name] -> A.Expr -> Gen Name S.SExpr
expr2sexpr ns = \case
  A.Var i -> return $ S.Var (ns !! i)
  A.Global n -> return $ S.Var n
  A.Lit l -> return $ S.Lit (lit2slit l)
  A.App (A.Var i) n -> return $ S.App (ns !! i) [expr2atom ns n]
  A.LetRec bs e -> undefined
