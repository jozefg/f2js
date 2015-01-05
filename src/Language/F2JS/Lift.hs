{-# LANGUAGE LambdaCase #-}
module Language.F2JS.Lift where
import Language.F2JS.AST
import Language.F2JS.Util

-- | Propogate the bindings on in a function and argument
-- up a level.
mergeApp :: Expr -> Expr -> Expr
mergeApp (LetRec bs e) l@(LetRec bs' _) =
  let LetRec bs'' e'' = succExprFrom (negate $ length bs') (length bs) l
  in LetRec (bs ++ bs'') (App e e'')
mergeApp (LetRec bs e) r =
  LetRec bs $ App e (succExpr (length bs) r)
mergeApp l (LetRec bs e) =
  LetRec bs $ App (succExpr (length bs) l) e
mergeApp l r = App l r

-- | Propogate bindings in record fields up a level. This is slow and
-- stupid now but it's simple *shrug*.
mergeRecord :: [(Name, Expr)] -> Expr
mergeRecord rs =
  let l = length rs
      newRec = zip (map fst rs) (map Var [0..])
      binds = map (Bind Nothing . succExpr l) (map snd rs)
  in LetRec binds (Record newRec)

mergeCon :: Tag -> [Expr] -> Expr
mergeCon t es =
  let l = length es
      binds = map (Bind Nothing . succExpr l) es
  in LetRec binds (Con t $ map Var [0..l - 1])

-- | Propogate all lambdas into explicit LetRec's.
-- This function bubbles up and tries to merge different
-- let bindings into a single LetRec.
lambdaLift :: Expr -> Expr
lambdaLift = \case
  Var i -> Var i
  Global n -> Global n
  Lit l -> Lit l
  Con t es -> mergeCon t (map lambdaLift es)
  PrimOp p -> PrimOp p
  Record rs -> mergeRecord (map (fmap lambdaLift) rs)
  Proj e n -> case lambdaLift e of
               LetRec bs e' -> LetRec bs (Proj e' n)
               _ -> Proj e n
  LetRec bs e -> LetRec (map liftB bs) (lambdaLift e)
  App l r -> mergeApp (lambdaLift l) (lambdaLift r)
  Case e alts -> Case (lambdaLift e) (map (fmap lambdaLift) alts)
  Lam c e -> LetRec [Bind c . Lam c $ skipLambdas e] (Var 0)
  where liftB (Bind c e) = Bind c (skipLambdas e)
        skipLambdas (Lam c e) = Lam c (skipLambdas e)
        skipLambdas e = lambdaLift e

-- | Convert @F ComplicatedExpr@ into @LetRec [ComplicatedExpr] F (Var 0)@.
-- This is needed to convert to STG.
deExp :: Expr -> Expr
deExp = \case
  Record rs -> Record $ map (fmap deExp) rs
  Proj e n -> case deExp e of
               LetRec bs e' -> LetRec bs (Proj e' n)
               _ -> Proj e n
  LetRec bs e -> LetRec (map liftB bs) (deExp e)
  App l r ->
    let r' = deExp r
        l' = deExp l
    in case deExp r of
        Lit{} -> mergeApp l' r'
        Var{} -> mergeApp l' r'
        Global{} -> mergeApp l' r'
        _     -> LetRec [Bind Nothing $ succExpr 1 r'] $
                 mergeApp (succExpr 1 l') (Var 0)
  Case e alts -> Case (deExp e) (map (fmap deExp) alts)
  Var i -> Var i
  Global n -> Global n
  Lit l -> Lit l
  Con t es -> mergeCon t (map deExp es)
  PrimOp p -> PrimOp p
  Lam c e -> Lam c (deExp e)
  where liftB (Bind c e) = Bind c (deExp e)

liftDecls :: [Decl] -> [Decl]
liftDecls = map go
  where go f@Foreign{} = f
        go (TopLevel n i e) = TopLevel n i (deExp . lambdaLift $ e)
