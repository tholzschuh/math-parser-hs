module Main where

import Lexer
import Parser
import Evaluator

import System.IO
import System.Environment
import qualified Data.Map as M
import Control.Monad.State




main' :: IO ()
main' = do
       (f:_)   <- getArgs
       content <- readFile f
       loop (M.fromList [("pi", pi), ("e", exp 1)]) (lines content)       

loop :: SymTab -> [String] -> IO ()
loop _ [] = return ()
loop symTab (t:ts) = 
                     let toks = tokenize t
                         nodes = parse toks
                         eval = evaluate nodes
                         (val, symTab') = runState eval symTab
                      in 
                         do
                         putStrLn $ show val
                         loop symTab' ts 
       

main :: IO ()
main = do
       loop' $ M.fromList [("pi", pi), ("e", exp 1)]

loop' :: SymTab -> IO ()
loop' symTab = do
            str <- getLine
            if null str 
            then
                return ()
            else 
                let toks = tokenize str
                    nodes = parse toks
                    eval = evaluate nodes
                    (val, symTab') = (runState eval) symTab
                in 
                   do
                   putStrLn $ show val
                   loop' symTab'
