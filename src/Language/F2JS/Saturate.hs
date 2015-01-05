{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
module Language.F2JS.Saturate where
import           Data.List
import qualified Data.Map           as M
import           Language.F2JS.AST
import           Language.F2JS.Util

type JSMap = M.Map Name Int

-- | Check to see whether the expr is a chain of applications
-- terminating in a global name. If this is the case it returns the
-- global name and the length of the chain.
appChain :: Expr -> Maybe (Name, Int)
appChain (App l _) | Just (n, i) <- appChain l = Just (n, i + 1)
appChain (Global n) = Just (n, 0)
appChain _ = Nothing

-- | Given an expression that needs to be applied to N more arguments,
-- eta expand it so it is fully saturated.
saturate :: Int -> Expr -> Expr
saturate i e =
  let e' = succExpr i e
      vars = map Var [i, i - 1 .. 0]
  in abstract i (foldl' App e' vars)
  where abstract 0 !e = e
        abstract n !e = abstract (n - 1) (Lam Nothing e)

-- | In an expression, saturate all foreign calls and primop
-- applications so they're fully applied. This needs the arity of all
-- foreign calls, these are stored in a 'JSMap'.
saturateExpr :: JSMap -> Expr -> Expr
saturateExpr jsm = \case
  -- Expand JS calls and primops
  e | Just (n, i) <- appChain e
    , Just j <- M.lookup n jsm
    , i < j -> saturate (j - i) (goChain e)
  App (App p@PrimOp{} l) r -> App (App p $ go l) (go r)
  App p@PrimOp{} r -> Lam Nothing $ App (App p (succExpr 1 $ go r)) (Var 0)
  p@PrimOp{} -> Lam Nothing . Lam Nothing $ App (App p $ Var 1) (Var 0)
  -- And recurse everywhere else
  Proj e n -> Proj (go e) n
  Record ns -> Record $ map (fmap go) ns
  LetRec binds e -> LetRec (map goBind binds) (go e)
  Lam c e -> Lam c (go e)
  App l r -> App (go l) (go r)
  Case e alts -> Case (go e) (map (fmap go) alts)
  Con t es -> Con t (map go es)
  e -> e
  where go = saturateExpr jsm
        -- | Expand all the arguments to a function call chain
        goChain (App l r) = App (goChain l) (go r)
        goChain e = e
        goBind (Bind clos expr) = Bind clos (go expr)

-- | Build up a map of JS arities from a list of declarations.
buildJSMap :: [Decl] -> JSMap
buildJSMap = foldl' go M.empty
  where go jsm Foreign {..} = M.insert jsName jsArity jsm
        go jsm _ = jsm

-- | Saturate all primitive and foreign applications
saturateDecls :: [Decl] -> [Decl]
saturateDecls decs = map go decs
  where go f@Foreign{} = f
        go (TopLevel n bound e) =
          TopLevel n bound (saturateExpr jsm e)
        jsm = buildJSMap decs
