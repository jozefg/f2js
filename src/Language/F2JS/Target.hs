module Language.F2JS.Target where
import Language.F2JS.Util

data UpdateFlag = Update | NoUpdate

data Decl = Decl { declName :: Name
                 , declClos :: Closure }

data Closure = Closure { closFlag :: UpdateFlag
                       , closClos :: [Name]
                       , closArgs :: [Name]
                       , closBoxy :: Either String SExpr }

data Lit = String String | Double Double | Record [(Name, Atom)] | Bool Bool
data Atom = NameAtom Name | LitAtom Lit

data Pat = LitPat Lit
         | RecordPat [(Name, Name)] -- Bind the field at LNAME -> RNAME
         | WildPat
         | ConPat Tag [Name]
         | BindPat Name

data SExpr = Let [Closure] SExpr
           | App Name [Atom]
           | Con Tag [Atom]
           | Prim PrimOp [Atom]
           | Case SExpr [(Pat, SExpr)]
           | Lit Lit
           | Var Name
           | Proj SExpr Name