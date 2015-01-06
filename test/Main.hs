{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.F2JS

(#) :: Expr -> Expr -> Expr
(#) = app
infixl 5 #

program :: [Decl]
program = [ define "foo" 1 $ lit 1 + 0
          , define "bar" 0 $ con (tag 1) ["foo" `app` lit 1]
          , define "main" 0 $ match "bar" [(conPat (tag 1) 1, 0)]]

factorial :: [Decl]
factorial = [ jsForeign "print" 1 "console.log"
            , define "factorial" 2 $
              match 0 [ (litPat 0, var 1)
                      , (wildPat,
                         "factorial" # (var 0 - lit 1) # (var 0 * var 1))]
            , define "main" 0 $ "print" # ("factorial" # lit 2 # ("factorial" # lit 5 # lit 1))]

main :: IO ()
main = compileProgram factorial CompilerConfig { rtsLoc = "rts/rts.js"
                                               , outLoc = "out.js" }
