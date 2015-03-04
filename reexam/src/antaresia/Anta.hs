-- | This module defines a simple command line interface for the Antaresia
-- interpreter.  If your solution is correct, this module should just
-- work.
module Main
       (main)
where
import AntaAST
import AntaParser
import AntaInterpreter
import Control.Monad(forM_)
import Data.List(intercalate)

import System.Environment

-- | nice display of Antaresia values
nice :: Value -> String
nice (IntVal v) = show v
nice TrueVal = "True"
nice FalseVal = "False"
nice (List vs) = "["++ intercalate ", " (map nice vs) ++"]"

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> error $ show e
                Right prog ->
                  case runProg prog of
                    Left e -> error $ show e
                    Right res -> forM_ res (\(n,v) -> putStrLn $ n ++ " = " ++ nice v)
            _ ->
              error "Give me a (single) argument!"
