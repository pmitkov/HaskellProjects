module Main where

import Symbolic

main :: IO ()
main = do input <- readREPL
          unless (input == "exit") (putStrLn (show $ evalExpr input)) 
          main

readREPL :: IO String
readREPL = do putStr "SYMBOLIC> "
              hFlush stdout
              getLine