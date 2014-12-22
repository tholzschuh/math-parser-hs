module Evaluator 
       ( SymTab
       , Evaluator 
       , lookUp
       , addSymbol
       , evaluate
       ) where
       

import Lexer
import Parser

import qualified Data.Map as M

import Control.Monad.State

type SymTab = M.Map String Double

type Evaluator a = State SymTab a

lookUp :: Identifier -> Evaluator Double
lookUp sym = state $ (\symTab ->
                   case M.lookup sym symTab of
                     (Just x) -> (x, symTab)
                     Nothing  -> error $ "Unknown symbol: " ++ sym)

addSymbol :: Identifier -> Double -> Evaluator Double 
addSymbol var x = state $ \symTab ->
                  let symTab' = M.insert var x symTab
                  in (x, symTab')


evaluate :: NodeT -> Evaluator Double
evaluate (SumNode op left right) = evaluate left >>= \x ->
                                   evaluate right >>= \x' ->
                                   case op of
                                     Plus  -> return $ x + x'
                                     Minus -> return $ x - x'
                                     _     -> error $ "Invalid operator in SumNode: " ++ show op

evaluate (ProductNode op left right) = evaluate left >>= \x ->
                                      evaluate right >>= \x' ->
                                      case op of
                                        Times -> return $ x * x'
                                        Div   -> return $ x / x'
                                        _     -> error $ "Invalid operator in ProductNode: " ++ show op
evaluate (AssignNode var node) = evaluate node >>= \x ->
                                 addSymbol var x

evaluate (UnaryNode op node) = evaluate node >>= \x ->
                               case op of
                                 Plus  -> return x
                                 Minus -> return (-x)
                                 _     -> error  $ "Invalid operator in UnaryNode: " ++ show op

evaluate (NumNode n) = return n

evaluate (VarNode var) = lookUp var


