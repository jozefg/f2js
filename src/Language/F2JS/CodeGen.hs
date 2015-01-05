{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.F2JS.CodeGen (jsify) where
import           Control.Applicative
import           Language.F2JS.Target
import           Language.F2JS.Util
import qualified Language.JavaScript.AST          as J
import qualified Language.JavaScript.NonEmptyList as J

-- | A compiled version of a closure. Critically arguments where
-- folded into body, namely into the appropriate preamble.
data CompiledClosure = CClosure { cclosName :: J.Name
                                , cclosFlag :: Bool
                                , cclosClos :: [J.Name]
                                , cclosBody :: J.Expr }

shouldUpdate :: UpdateFlag -> Bool
shouldUpdate = \case
  Update -> True
  NoUpdate -> False

jname :: String -> J.Name
jname = either error id . J.name

jvar :: Name -> J.Name
jvar (Gen i) = jname ('_' : show i)
jvar (Str s) = jname ('_' : s)

-- | Return an expression
ret :: J.Expr -> J.Stmt
ret = J.StmtDisruptive . J.DSReturn . J.ReturnStmt . Just

-- | Bind a var to value
var :: J.Name -> J.Expr -> J.VarStmt
var l r = J.VarStmt . J.singleton $ J.VarDecl l (Just r)

-- | Convert an atom to the appropriate closure. For a @Name@ we don't
-- introduce a new closure, it should already be bound to one. For an atom
-- we call 'lit' to introduce a new closure.
atom :: Atom -> J.Expr
atom (NameAtom n) = J.ExprName (jvar n)
atom (LitAtom l) = mkLit (lit l)

mkLit :: J.Expr -> J.Expr
mkLit e = J.ExprName (jname "mkLit") `J.ExprInvocation` J.Invocation [e]

-- | Construct a closure for a literal expression.
lit :: Lit -> J.Expr
lit = J.ExprLit <$> \case
  Double d -> J.LitNumber (J.Number d)
  String s -> J.LitString (either error id $ J.jsString s)
  Bool b -> J.LitNumber . J.Number $ if b then 1 else 0
  Record r ->
    J.LitObject
    . J.ObjectLit
    . map (uncurry (J.ObjectField . Left . jvar) . fmap atom)
    $ r

-- | Enter a closure and return the result. This relies on the RTS
-- @enter@ function. The return is needed to facilate trampolining.
--
-- NOTE: ONLY CALL THIS AT THE END OF A JS FUNCTION
enter :: J.Expr -> J.Stmt
enter e =
  let call = J.ExprInvocation (J.ExprName $ jname "enter")
                              (J.Invocation [e])
  in ret call

-- | Push an expression onto the appropriate stack.
pushStack :: J.Name -> J.Expr -> J.Stmt
pushStack nm e =
  J.StmtExpr $
  J.singleton (J.LValue nm [([], J.Property $ jname "push")])
  `J.ESApply` (J.RVInvoke . J.singleton . J.Invocation) [e]

pushArg :: J.Expr -> J.Stmt
pushArg = pushStack (jname "ARG_STACK")

pushEval :: J.Expr -> J.Stmt
pushEval = pushStack (jname "EVAL_STACK")

pushCont :: J.Expr -> J.Stmt
pushCont = pushStack (jname "CONT_STACK")

evalCont :: J.Expr
evalCont = J.ExprName (jname "evalFirst")

-- | Push all arguments on to the @ARG_STACK@ and enter the function
-- closure.
app :: J.Name -> [J.Expr] -> [J.Stmt]
app f args = reverse (map pushArg args) ++ [enter $ J.ExprName f]

-- | Returns the appropriate RTS op for a primop
primCont :: PrimOp -> J.Expr
primCont = J.ExprName . jname <$> \case
  Plus -> "plusPrim"
  Times -> "multPrim"
  Divide -> "divPrim"
  Minus -> "minusPrim"
  Modulo -> "modPrim"
  ShL -> "shlPrim"
  ShR -> "shrPrim"
  CmpEQ -> "eqPrim"
  CmpLT -> "ltPrim"
  CmpGT -> "gtPrim"
  CmpLE -> "ltePrim"
  CmpGE -> "gtePrim"

-- | Evaluate the left and right expressions and jump to the
-- appropriate RTS call for the primop
primOp :: PrimOp -> J.Expr -> J.Expr -> [J.Stmt]
primOp p l r = [ pushCont (primCont p)
               , pushCont evalCont
               , pushArg r
               , enter l]

-- | A continuation to project out a certain field
projCont :: J.Name -> J.Expr
projCont n = J.ExprName (jname "project")
             `J.ExprInvocation` J.Invocation [J.ExprName n]

nextArg :: J.Expr
nextArg =
  let f = J.ExprName (jname "ARG_STACK") `J.ExprRefinement`
          J.Property (jname "pop")
  in f `J.ExprInvocation` J.Invocation []

closedAt :: Int -> J.Expr
closedAt i =
  J.ExprName (jname "CURRENT_CLOS") `J.ExprRefinement`
  J.Property (jname "clos") `J.ExprRefinement`
  J.Subscript (J.ExprLit . J.LitNumber . J.Number $ fromIntegral i)

-- | Make a closure. This relies on the rts call @mkClos@
mkClos :: Bool -> [J.Expr] -> J.Expr -> J.Expr
mkClos b cs e =
  J.ExprName (jname "mkClosure") `J.ExprInvocation`
  J.Invocation [J.ExprLit $  J.LitNumber . J.Number $ if b then 1 else 0
               , list
               , e]
  where list = J.ExprLit . J.LitArray . J.ArrayLit $ cs

-- | Bind all the closed variables and argument variables to the
-- appropriate names before running the body.
entryCode :: [J.Name] -- ^ Closed variables
             -> [J.Name] -- ^ Argument variables
             -> [J.Stmt] -- ^ Body code
             -> J.FnBody
entryCode cs as body =
  let bindings = map bindArgVar as ++ zipWith bindClosVar cs [0..]
  in J.FnBody bindings body
  where bindArgVar n = var n nextArg
        bindClosVar n i = var n (closedAt i)

-- | Reset the closed over variables of a closure. We need this to
-- properly implement letrec since JS doesn't support recursie values.
setClosed :: J.Name -> [J.Name] -> J.Stmt
setClosed n ns = J.StmtExpr -- Good god.
                 . J.ESApply (J.singleton $
                              J.LValue n [([], J.Property $ jname "clos")])
                 . J.RVAssign
                 . J.ExprLit
                 . J.LitArray
                 . J.ArrayLit
                 $ map J.ExprName ns

-- | An unfortunately CPSed implementation of letrec. This binds all
-- the closures without their closed over variables and then in a
-- separate pass adds them back in. This 2 pass step is how Scheme
-- implementers often implement letrec so it seems legit to me.
letrec :: [CompiledClosure] -> [J.Stmt] -> J.Stmt
letrec closes body =
  ret . run . J.LitFn . J.FnLit Nothing [] $ J.FnBody bs (setCloses ++ body)
  where bs = map bind closes
        run fnlit = J.ExprInvocation (J.ExprLit fnlit) (J.Invocation [])
        setCloses = map set closes
        bind CClosure{..} = var cclosName (mkClos cclosFlag [] cclosBody)
        set CClosure{..} = setClosed cclosName cclosClos

-- | Construct a tagged value.
con :: Tag -> [J.Expr] -> J.Expr
con (Tag i) es = J.ExprName (jname "mkCon")
                 `J.ExprInvocation` J.Invocation [t, args]
  where t = J.ExprLit . J.LitNumber . J.Number $ fromIntegral i
        args = J.ExprLit . J.LitArray . J.ArrayLit $ es

matcher :: J.Expr -> J.Expr -> J.Expr
matcher l r = J.ExprLit
              . J.LitObject
              . J.ObjectLit
              $ [ J.ObjectField (Left $ jname "pred") l
                , J.ObjectField (Left $ jname "cont") r]

-- | Create the object representing an alternative of a case
-- expression. This is composed of two fields, a predicate and a
-- cont. If a predicate matches the cont is executed with the matchee
-- as an argument.
alt :: Pat -> J.FnBody -> J.Expr
alt (ConPat (Tag i) ns) (J.FnBody vs ss) = matcher m fn
  where m = J.ExprName (jname "matchTag") `J.ExprInvocation`
            J.Invocation [J.ExprLit . J.LitNumber . J.Number $ fromIntegral i]
        fn = J.ExprLit
             . J.LitFn
             . J.FnLit Nothing [jname "x"]
             $ J.FnBody (vs ++ vs') ss
        vs' = zipWith setSub (map jvar ns) [0..]
        setSub n i =
          var n $
          J.ExprName (jname "x")
          `J.ExprRefinement` (J.Property $ jname "args")
          `J.ExprRefinement`
          (J.Subscript . J.ExprLit . J.LitNumber . J.Number . fromIntegral) i

alt (LitPat l) fnlit = matcher m fn
  where m = J.ExprName (jname "matchLit") `J.ExprInvocation`
            J.Invocation [lit l]
        fn = J.ExprLit . J.LitFn . J.FnLit Nothing [jname "x"] $ fnlit
alt WildPat fnlit = matcher m fn
  where m = J.ExprName (jname "matchAll")
        fn = J.ExprLit . J.LitFn . J.FnLit Nothing [jname "x"] $ fnlit

-- | Call the rts function to glue together a bunch of alternatives.
matchCont :: [(Pat, J.FnBody)] -> J.Expr
matchCont ms = J.ExprName (jname "matcher") `J.ExprInvocation`
               J.Invocation [arr]
  where arr = J.ExprLit . J.LitArray . J.ArrayLit $ map (uncurry alt) ms

mkForeign :: Int -> String -> [J.Stmt]
mkForeign i code =
  (:[]) . ret $
  J.ExprName (jname "mkForeign")
  `J.ExprInvocation` J.Invocation [arity, J.ExprName $ jname code]
  where arity = J.ExprLit . J.LitNumber . J.Number . fromIntegral $ i

compileClos :: Decl -> CompiledClosure
compileClos (Decl n Closure{..}) =
  CClosure { cclosName = jvar n
           , cclosFlag = shouldUpdate closFlag
           , cclosClos = map jvar closClos
           , cclosBody = J.ExprLit
                         . J.LitFn
                         . J.FnLit Nothing []
                         . entryCode (map jvar closClos) (map jvar closArgs)
                         . either (mkForeign $ length closArgs) expr
                         $ closBody}

fnBody :: SExpr -> J.FnBody
fnBody = J.FnBody [] . expr

expr :: SExpr -> [J.Stmt]
expr = \case
  Var n -> [enter (J.ExprName $ jvar n)]
  App n as -> app (jvar n) (map atom as)
  Prim p [l, r] -> primOp p (atom l) (atom r)
  Prim {} -> error "Unsaturated primop"
  Proj e n -> pushCont (projCont $ jvar n) : expr e
  Lit l -> [enter . mkLit $ lit l]
  Con t as -> [enter $ con t (map atom as)]
  Let ds e -> [letrec (map compileClos ds) (expr e)]
  Case e alts -> (pushCont . matchCont $ map (fmap fnBody) alts) : expr e

jsify :: [Decl] -> [J.VarStmt]
jsify = map go
  where go (Decl nm Closure{..}) =
          var (jvar nm)
          . mkClos (shouldUpdate closFlag) (arr closClos)
          . J.ExprLit
          . J.LitFn
          . J.FnLit Nothing []
          . entryCode (map jvar closClos) (map jvar closArgs)
          . either (mkForeign $ length closArgs) expr
          $ closBody
        arr = map (J.ExprName . jvar)
