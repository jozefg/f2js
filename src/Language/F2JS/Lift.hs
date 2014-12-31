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
  let newRec = zip (map fst rs) (map Var [0..])
      binds = map (Bind Nothing) (map snd rs)
  in LetRec binds (Record newRec)

-- | Propogate all lambdas into explicit LetRec's.
-- This function bubbles up and tries to merge different
-- let bindings into a single LetRec.
lambdaLift :: Expr -> Expr
lambdaLift = \case
  Var i -> Var i
  Global n -> Global n
  Lit l -> Lit l
  Con t -> Con t
  PrimOp p -> PrimOp p
  Record rs -> mergeRecord (map (fmap lambdaLift) rs)
  Proj e n -> case lambdaLift e of
               LetRec bs e' -> LetRec bs (Proj e' n)
               _ -> Proj e n
  LetRec bs e -> LetRec (map liftB bs) (lambdaLift e)
  App l r -> mergeApp (lambdaLift l) (lambdaLift r)
  Case e alts -> Case (lambdaLift e) (map (fmap lambdaLift) alts)
  Lam c e -> LetRec [Bind c $ Lam c e] (Var 0)
  where liftB (Bind c e) = Bind c (lambdaLift e)
