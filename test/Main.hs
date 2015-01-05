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
factorial = [ define "factorial" 2 $
              match 0 [ (litPat 0, 1)
                      , (wildPat, "factorial" # (0 - lit 1) # (0 * 1))]
            , define "main" 0 $ "factorial" # lit 5 # lit 1]

main :: IO ()
main = compileProgram factorial $ CompilerConfig { rtsLoc = "/home/jozefg/f2js/rts/rts.js"
                                               , outLoc = "out.js" }
