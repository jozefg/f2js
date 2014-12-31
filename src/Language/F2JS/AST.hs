{-# LANGUAGE LambdaCase #-}
module Language.F2JS.AST where
import qualified Data.Set as S
import Data.Foldable (foldMap)
import Language.F2JS.Util

data Lit = String String
         | Double Double
         | Bool Bool
         deriving Show

type Closure = [Int]

data Bind = Bind { closure :: Maybe Closure
                 , body    :: Expr }
          deriving Show

data Expr = Var Int
          | Global Name
          | Lit Lit
          | Con Tag
          | PrimOp PrimOp
          | Record [(Name, Expr)]
          | Proj Expr Name
          | LetRec [Bind] Expr
          | Lam (Maybe Closure) Expr
          | App Expr Expr
          | Case Expr [(Pat, Expr)]
          deriving Show

data Pat = LitPat Lit
         | RecordPat [Name] -- Bind the field at NAME -> POS IN LIST
         | WildPat
         | ConPat Tag Int
         deriving Show

data Decl = Foreign { jsName  :: Name
                    , jsArity :: Int
                    , jsCode  :: String }
          | TopLevel Name [Name] (Maybe Closure) Expr
          deriving Show

succExpr :: Int -> Expr -> Expr
succExpr = succExprFrom 0

succExprFrom :: Int -> Int -> Expr -> Expr
succExprFrom i inc = \case
  Var j -> Var $ bump i inc j
  Record rs -> Record $ map (fmap $ go i inc) rs
  Proj l r -> Proj (go i inc l) r
  LetRec binds e -> let i' = i + length binds
                    in LetRec (map (goBind i' inc) binds)
                       (go i' inc e)
  Lam c e -> Lam (fmap (map $ bump i inc) c) (go (i + 1) inc e)
  App l r -> App (go i inc l) (go i inc r)
  Case e alts -> Case (go i inc e) $ map (goPat i inc) alts
  e -> e
  where go = succExprFrom
        bump i inc j = if j < i then j else j + inc
        goBind i inc (Bind clos expr) =
          Bind (fmap (map (bump i inc)) clos)
               (go i inc expr)
        goPat i inc (LitPat l, e) = (LitPat l, go i inc e)
        goPat i inc (WildPat, e) = (WildPat, go i inc e)
        goPat i inc (ConPat t j, e) = (ConPat t j, go (i + j) inc e)
        goPat i inc (RecordPat ns, e) =
          (RecordPat ns, go (length ns + i) inc e)


freeVars :: Expr -> S.Set Int
freeVars = \case
  Var i -> S.singleton i
  Record rs -> foldMap freeVars (fmap snd rs)
  Proj e _ -> freeVars e
  LetRec binds b -> prune (length binds)
                    $ foldMap freeVars (b : map body binds)
  Lam _ e -> prune 1 $ freeVars e
  App l r -> freeVars l `S.union` freeVars r
  Case e alts -> freeVars e `S.union` foldMap freePat alts
  _ -> S.empty
  where prune n = S.map (subtract n) . S.filter (>= n)
        freePat (LitPat _, e) = freeVars e
        freePat (WildPat, e) = freeVars e
        freePat (ConPat _ i, e) = prune i $ freeVars e
        freePat (RecordPat ns, e) = prune (length ns) $ freeVars e
