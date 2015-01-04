{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.F2JS

program :: [Decl]
program = [ define "foo" 1 $ lit 1 + 0
          , define "bar" 0 $ con (tag 1) ["foo" `app` lit 1]
          , define "main" 0 $ match "bar" [(conPat (tag 1) 1, 0)]]

main :: IO ()
main = compileProgram program $ CompilerConfig { rtsLoc = "/home/jozefg/f2js/rts/rts.js"
                                               , outLoc = "../out.js" }
