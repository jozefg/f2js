{-# LANGUAGE LambdaCase #-}
module Language.F2JS.AST where
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
succExpr = go 0
  where go i inc = \case
          Var j -> Var $ if j < i then j else j + inc
          Record rs -> Record $ map (fmap $ go i inc) rs
          Proj l r -> Proj (go i inc l) r
          LetRec binds e -> let i' = i + length binds
                            in LetRec (map (goBind i' inc) binds)
                                      (go i' inc e)
          Lam c e -> Lam (fmap (map $ bump i inc) c) (go (i + 1) inc e)
          App l r -> App (go i inc l) (go i inc r)
          Case e alts -> Case (go i inc e) $ map (goPat i inc) alts
          e -> e
        bump i inc j = if j < i then j else j + inc
        goBind i inc (Bind clos expr) =
          Bind (fmap (map (+ inc)) clos)
               (go i inc expr)
        goPat i inc (LitPat l, e) = (LitPat l, go i inc e)
        goPat i inc (WildPat, e) = (WildPat, go i inc e)
        goPat i inc (ConPat t j, e) = (ConPat t j, go (i + j) inc e)
        goPat i inc (RecordPat ns, e) =
          (RecordPat ns, go (length ns + i) inc e)
