module Language.F2JS.AST where

data Name = Str String | Gen Int

data Prim = String String
          | Double Double

data Expr = Var Int
          | Glob Name
          | Prim Prim
          | Record [(Name, Expr)]
          | Lam Expr
          | LetRec [(Name, Expr)]
          | Ap Expr Expr
          | Case [(Pat, Expr)]

data Pat = PrimPat Prim
         | RecordPat [(Name, Pat)]
         | WildPat

data Decl = Foreign String
          | TopLevel Name [Name] Expr
