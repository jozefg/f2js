{-# LANGUAGE LambdaCase #-}
module Language.F2JS.Closure where
import qualified Data.Set as S
import Language.F2JS.AST

annClos :: Expr -> Expr
annClos = \case
  Record ns -> Record (map (fmap annClos) ns)
  Proj e n -> Proj (annClos e) n
  LetRec binds e -> LetRec (map annB binds) (annClos e)
  l@(Lam _ e) -> Lam (clos l) (annClos e)
  App l r -> App (annClos l) (annClos r)
  Case e bs -> Case (annClos e) (map (fmap annClos) bs)
  e -> e
  where clos = Just . S.toList . freeVars
        annB (Bind _ e) = Bind (clos e) e
