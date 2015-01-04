module Language.F2JS.MakeMain where
import Language.JavaScript.AST
import Language.JavaScript.NonEmptyList

jname :: String -> Name
jname = either error id . name

enterMain :: Stmt
enterMain =
  StmtExpr $
  singleton (LValue (jname "enterMain") []) `ESApply`
  (RVInvoke . singleton . Invocation) []

makeMain :: [VarStmt] -> Program
makeMain vs = Program vs [enterMain]
