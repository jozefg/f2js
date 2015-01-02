module Language.F2JS.Pipeline where
import qualified Language.F2JS.AST as A
import qualified Language.F2JS.Target as S

import Language.F2JS.Saturate (saturateDecls)
import Language.F2JS.Lift (liftDecls)
import Language.F2JS.Closure (closureConvert)
import Language.F2JS.STGify (stgify)

pipeline :: [A.Decl] -> [S.Decl]
pipeline = stgify . closureConvert . liftDecls . saturateDecls