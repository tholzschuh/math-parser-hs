module Lexer 
       ( Operator(..)
       , Token(..)
       , operator
       , number
       , identifier
       , tokenize
       ) where

import Data.Char

data Operator = Plus | Minus | Times | Div deriving (Show, Eq)

data Token = TokOp Operator
           | TokNum Double
           | TokIdent String
           | TokAssign
           | TokLParen
           | TokRParen
           | TokSpace
           | TokEnd
           deriving (Show, Eq)

operator :: Char -> Operator
operator c
         | c == '+' = Plus
         | c == '-' = Minus
         | c == '*' = Times
         | c == '/' = Div
         | otherwise = error $ "Unknown operator: " ++ [c]

number :: Char -> String -> [Token]
number c cs = let (c', cs') = span isDigit cs
              in TokNum (read $ c:c') : tokenize cs'

identifier :: Char -> String -> [Token]
identifier c cs = let (c', cs') = span isAlphaNum cs
                  in TokIdent (c:c') : tokenize cs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) 
              | c `elem` "+-*/" = TokOp (operator c) : tokenize cs
              | isDigit c = number c cs 
              | isAlpha c = identifier c cs
              | isSpace c = TokSpace : tokenize cs
              | c == '=' = TokAssign : tokenize cs
              | c == '(' = TokLParen : tokenize cs
              | c == ')' = TokRParen : tokenize cs
              | otherwise = error $ "Unable to tokenize: " ++ [c]
