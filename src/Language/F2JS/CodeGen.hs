{-# LANGUAGE LambdaCase #-}
module Language.F2JS.CodeGen where
import           Control.Applicative
import           Language.F2JS.Target
import           Language.F2JS.Util
import qualified Language.JavaScript.AST          as J
import qualified Language.JavaScript.NonEmptyList as J

jname :: String -> J.Name
jname = either error id . J.name

jvar :: Name -> J.Name
jvar (Gen i) = jname ('_' : show i)
jvar (Str s) = jname s

-- | Convert an atom to the appropriate closure. For a @Name@ we don't
-- introduce a new closure, it should already be bound to one. For an atom
-- we call 'lit' to introduce a new closure.
atom :: Atom -> J.Expr
atom (NameAtom n) = J.ExprName (jvar n)
atom (LitAtom l) = lit l

-- | Construct a closure for a literal expression. This relies on the
-- @mkLit@ RTS function.
lit :: Lit -> J.Expr
lit = mkLit . J.ExprLit <$> \case
  Double d -> J.LitNumber (J.Number d)
  String s -> J.LitString (either error id $ J.jsString s)
  Bool b -> J.LitBool b
  Record r ->
    J.LitObject
    . J.ObjectLit
    . map (uncurry $ J.ObjectField . Left . jvar)
    . map (fmap atom)
    $ r
  where mkLit e =
          J.ExprName (jname "mkLit") `J.ExprInvocation` J.Invocation [e]

-- | Enter a closure and return the result. This relies on the RTS
-- @enter@ function. The return is needed to facilate trampolining.
--
-- NOTE: ONLY CALL THIS AT THE END OF A JS FUNCTION
enter :: J.Name -> J.Stmt
enter e =
  let call = J.ExprInvocation (J.ExprName $ jname "enter")
                              (J.Invocation [J.ExprName $ e])
  in J.StmtDisruptive . J.DSReturn . J.ReturnStmt . Just $ call

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

-- | Push all arguments on to the @ARG_STACK@ and enter the function
-- closure.
app :: J.Name -> [J.Expr] -> [J.Stmt]
app f args = map pushArg args ++ [enter f]
