{-# LANGUAGE LambdaCase #-}
module Language.F2JS.STGify where
import Control.Applicative
import Control.Monad
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

-- | Unwrap the DB variables around a lambda
unwrapLambdas :: A.Expr -> Gen Name (A.Expr, [Name])
unwrapLambdas e = go e []
  where go (A.Lam _ body) ns = gen >>= go body . (: ns)
        go e ns = return (e, ns)

appChain :: [Name] -> A.Expr -> Maybe (Either Name PrimOp, [S.Atom])
appChain ns = go []
  where go as (A.App l r) = go (expr2atom ns r : as) l
        go as (A.Var i) = Just (Left $ ns !! i, as)
        go as (A.Global n) = Just (Left n, as)
        go as (A.PrimOp p) = Just (Right p, as)
        go _ _ = Nothing


expr2sexpr :: [Name] -> A.Expr -> Gen Name S.SExpr
expr2sexpr ns = \case
  A.Var i -> return $ S.Var (ns !! i)
  A.Global n -> return $ S.Var n
  e | Just (op, atoms) <- appChain ns e ->
        return $ case op of
        Right p -> S.Prim p atoms
        Left n -> S.App n atoms
  A.Lit l -> return $ S.Lit (lit2slit l)
  A.LetRec bs e -> do
    ns' <- (++ ns) <$> replicateM (length bs) gen
    S.Let <$> mapM (bind2clos ns') bs <*> expr2sexpr ns' e
  A.Con t es -> return $ S.Con t (map (expr2atom ns) es)
  A.Proj e n -> S.Proj <$> expr2sexpr ns e <*> pure n
  where bind2clos ns (A.Bind (Just c) e) = do
          (body, args) <- unwrapLambdas e
          body' <- expr2sexpr (args ++ ns) body
          return S.Closure { S.closFlag =
                                if null args then S.Update else S.NoUpdate
                           , S.closClos = map (ns !!) c
                           , S.closArgs = args
                           , S.closBoxy = Right body' }
