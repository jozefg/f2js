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

alt2salt :: [Name] -> (A.Pat, A.Expr) -> Gen Name (S.Pat, S.SExpr)
alt2salt ns (p, e) = do
  (p', ns') <- case p of
    A.LitPat l -> return (S.LitPat (lit2slit l), [])
    A.WildPat -> return (S.WildPat, [])
    A.ConPat t i -> (\ps -> (S.ConPat t ps, ps)) <$> replicateM i gen
    A.RecordPat rs ->
      (\ps -> (S.RecordPat $ zip rs ps, ps)) <$> replicateM (length rs) gen
  e' <- expr2sexpr (ns' ++ ns) e
  return (p', e')

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
    S.Let <$> mapM (bind2clos ns') (zip bs [0..]) <*> expr2sexpr ns' e
  A.Con t es -> return $ S.Con t (map (expr2atom ns) es)
  A.Proj e n -> S.Proj <$> expr2sexpr ns e <*> pure n
  A.Case e alts -> S.Case <$> expr2sexpr ns e <*> mapM (alt2salt ns) alts
  A.Record rs -> return . S.Lit . S.Record $ map (fmap $ expr2atom ns) rs
  where bind2clos ns (A.Bind (Just c) e, i) = do
          (body, args) <- unwrapLambdas e
          body' <- expr2sexpr (args ++ ns) body
          let flag = if null args then S.Update else S.NoUpdate
          return S.Decl { S.declName = ns !! i
                        , S.declClos =
                          S.Closure { S.closFlag = flag
                                    , S.closClos = map (ns !!) c
                                    , S.closArgs = args
                                    , S.closBoxy = Right body' }}

decl2sdecl :: A.Decl -> Gen Name S.Decl
decl2sdecl = \case
  A.Foreign nm arity code ->
    return . S.Decl nm $
    S.Closure S.NoUpdate [] (map Gen [0..arity]) (Left code)
  A.TopLevel nm i e -> do
    ns <- replicateM i gen
    se <- expr2sexpr ns e
    let flag = if i == 0 then S.Update else S.NoUpdate
    return $ S.Decl nm $ S.Closure flag [] ns (Right se)

stgify :: [A.Decl] -> [S.Decl]
stgify = runGenWith (successor s) (Gen 0) . mapM decl2sdecl
  where s (Gen i) = Gen (i + 1)
