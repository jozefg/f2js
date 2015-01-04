module Language.F2JS.Pipeline where
import qualified Language.F2JS.AST as A
import qualified Language.JavaScript.AST as J

import Language.F2JS.Saturate (saturateDecls)
import Language.F2JS.Lift (liftDecls)
import Language.F2JS.Closure (closureConvert)
import Language.F2JS.STGify (stgify)
import Language.F2JS.CodeGen (jsify)

pipeline :: [A.Decl] -> J.Program
pipeline = jsify . stgify . closureConvert . liftDecls . saturateDecls
