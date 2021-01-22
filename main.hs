module Main where

import Scanner (scan)
import Parser (parse)
import ContextAnalisis (wrap_Program,Inh_Program (..),Syn_Program (..))
--import AST (wrap_Program,Inh_Program (..),Syn_Program (..))
import System.Environment (getArgs)

main = do args <- getArgs
          let defaultFile = "A.java"     
          let origin = if null args then defaultFile else head args
          let destiny = if length args < 2 then ssmExt origin else args!!1 
          program <- readFile origin
          let symbols = scan program
          let parserResult = wrap_Program (parse symbols) Inh_Program
          let errors = messages_Syn_Program parserResult
          print "Founded symbols:"
          print symbols
          if not (null errors) 
           then do putStrLn "\nAST:"
                   print (ast_Syn_Program parserResult)
                   putStrLn "\nErrors:"
                   print (errors)
                   putStrLn "\nIn the code:"
                   putStrLn (errorProgram_Syn_Program parserResult)
           else do putStrLn "\nGenerated code:"
                   putStrLn (output_Syn_Program parserResult)
                   writeFile destiny (output_Syn_Program parserResult)
               
ssmExt :: String -> String
ssmExt name =  reverse (removeExt (reverse name)) ++ ".ssm"
    where removeExt s = case s of 
                             ('a':'v':'a':'j':'.':res) -> res
                             otherwise                 -> s

              
