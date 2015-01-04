{-# LANGUAGE RecordWildCards #-}
module Language.F2JS.Pipeline where
import qualified Language.F2JS.AST as A
import qualified Language.JavaScript.AST as J

import Language.F2JS.Saturate (saturateDecls)
import Language.F2JS.Lift (liftDecls)
import Language.F2JS.Closure (closureConvert)
import Language.F2JS.STGify (stgify)
import Language.F2JS.CodeGen (jsify)
import Language.F2JS.MakeMain (makeMain)
import Language.JavaScript.Pretty (pretty)
import System.Directory (copyFile)

pipeline :: [A.Decl] -> J.Program
pipeline = makeMain
           . jsify
           . stgify
           . closureConvert
           . liftDecls
           . saturateDecls

data CompilerConfig = CompilerConfig { rtsLoc :: FilePath
                                     , outLoc :: FilePath }

compileProgram :: [A.Decl] -> CompilerConfig -> IO ()
compileProgram as CompilerConfig{..} =
  let text = show . pretty $ pipeline as
  in do
    putStrLn "Compiling..."
    copyFile rtsLoc outLoc
    appendFile outLoc text
    putStrLn "Finished"
