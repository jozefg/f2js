module Language.F2JS.AST where
import Language.F2JS.Util

data Lit = String String
         | Double Double
         | Bool Bool
         deriving Show

data Expr = Var Int
          | Global Name
          | Lit Lit
          | Con Tag
          | Record [(Name, Expr)]
          | Proj Expr Name
          | LetRec [Expr] Expr
          | Lam Expr
          | App Expr Expr
          | Case Expr [(Pat, Expr)]
          deriving Show

data Pat = LitPat Lit
         | RecordPat [(Name, Name)] -- Bind the field at LNAME -> RNAME
         | WildPat
         | ConPat Tag [Name]
         | BindPat Name
         deriving Show

data Decl = Foreign { jsName  :: Name
                    , jsArity :: Int
                    , jsCode  :: String }
          | TopLevel Name [Name] Expr
          deriving Show
