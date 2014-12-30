module Language.F2JS.AST where

data Name = Str String | Gen Int
          deriving (Eq, Show, Ord)

data Prim = String String
          | Double Double
          deriving Show

data Expr = Var Int
          | Glob Name
          | Prim Prim
          | Record [(Name, Expr)]
          | Lam Expr
          | LetRec [(Name, Expr)]
          | Ap Expr Expr
          | Case [(Pat, Expr)]
          deriving Show

data Pat = PrimPat Prim
         | RecordPat [(Name, Pat)]
         | WildPat
         deriving Show

data Decl = Foreign String
          | TopLevel Name [Name] Expr
          deriving Show
