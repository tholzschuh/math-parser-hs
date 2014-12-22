module Parser 
       ( Identifier
       , NodeT(..)
       , lookAhead
       , accept
       , expression
       , term
       , factor
       , parse
       ) where

import Lexer

type Identifier = String

data NodeT = SumNode Operator NodeT NodeT
           | ProductNode Operator NodeT NodeT
           | AssignNode Identifier NodeT 
           | UnaryNode Operator NodeT
           | NumNode Double
           | VarNode Identifier
           deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept!"
accept (t:ts) = ts

expression :: [Token] -> (NodeT, [Token])
expression ts = let (termNode, ts') = term ts
                in 
                   case lookAhead ts' of
                     -- Term [+|-] Expression
                     (TokOp op) | op `elem` [Plus, Minus] ->
                                let (exprNode, ts'') = expression $ accept ts'
                                in (SumNode op termNode exprNode, ts'')
                     -- Identifier '=' Expression
                     TokAssign ->
                                case termNode of
                                     VarNode str ->
                                        let (exprNode, ts'') = expression $ accept ts'
                                        in (AssignNode str exprNode, ts'')
                     -- Term
                     _ -> (termNode, ts')

term :: [Token] -> (NodeT, [Token])
term ts = let (facNode, ts') = factor ts
          in
             case lookAhead ts' of
               -- Factor [*|/] Term
               (TokOp op) | op `elem` [Times, Div] -> 
                                      let (termNode, ts'') = term $ accept ts'
                                      in (ProductNode op facNode termNode, ts'')
               -- Factor
               _ -> (facNode, ts')
               

factor :: [Token] -> (NodeT, [Token])
factor ts = case lookAhead ts of
              -- Number
              (TokNum n) -> (NumNode n, accept ts) 
              -- Identifier
              (TokIdent str) -> (VarNode str, accept ts)
              -- [+|-] Factor
              (TokOp op) | op `elem` [Plus, Minus] ->
                         let (facNode, ts') = factor $ accept ts
                         in (UnaryNode op facNode, ts')
              -- '(' Expression ')'
              TokLParen ->
                      let (exprNode, ts') = expression $ accept ts
                      in 
                         if lookAhead ts' /= TokRParen
                         then error "Unbalanced paranthesis."
                         else (exprNode, accept ts')
              _ -> error $ "Parse error on token: " ++ show ts


parse :: [Token] -> NodeT
parse ts = let (exprNode, ts') = expression ts
           in
              if null ts' 
              then exprNode
              else error $ "Parse-Error. Left-over tokens: " ++ show ts'

