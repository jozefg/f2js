{-# LANGUAGE LambdaCase #-}
module Language.F2JS.Closure where
import Data.Foldable (foldMap)
import qualified Data.Set as S
import Language.F2JS.AST


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

closureConvert :: [Decl] -> [Decl]
closureConvert = map go
  where go f@Foreign{} = f
        go (TopLevel n i e) = TopLevel n i (annClos e)
