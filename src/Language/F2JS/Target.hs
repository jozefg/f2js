module Language.F2JS.Target where
import Language.F2JS.Util

data UpdateFlag = Update | NoUpdate
                deriving Show

data Decl = Decl { declName :: Name
                 , declClos :: Closure }
          deriving Show

data Closure = Closure { closFlag :: UpdateFlag
                       , closClos :: [Name]
                       , closArgs :: [Name]
                       , closBoxy :: Either String SExpr }
             deriving Show

data Lit = String String | Double Double | Record [(Name, Atom)] | Bool Bool
         deriving Show
data Atom = NameAtom Name | LitAtom Lit
          deriving Show

data Pat = LitPat Lit
         | RecordPat [(Name, Name)] -- Bind the field at LNAME -> RNAME
         | WildPat
         | ConPat Tag [Name]
         | BindPat Name
         deriving Show

data SExpr = Let [Decl] SExpr
           | App Name [Atom]
           | Con Tag [Atom]
           | Prim PrimOp [Atom]
           | Case SExpr [(Pat, SExpr)]
           | Lit Lit
           | Var Name
           | Proj SExpr Name
           deriving Show
