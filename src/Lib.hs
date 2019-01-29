module Lib
    ( evaluate,
      buildTree,
      getTokens,
      roundResult
    ) where

import Data.Char
import System.Exit
import Text.Printf
import System.Environment

data Operator = Plus | Minus | Times | Div | Power | Err String
    deriving (Show, Eq)

data Token = TokOp Operator
            | TokLParen
            | TokRParen
            | TokNum Double
            | TokEnd
            | TokErr String
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
            | c == '-' = Minus
            | c == '*' = Times
            | c == '/' = Div
            | c == '^' = Power
            | otherwise = Err $ "This operator is not valid: " ++ [c]

getTokens :: String -> [Token]
getTokens [] = []
getTokens (tok : list)
    | tok `elem` "+-*/^" = TokOp (operator tok) : getTokens list
    | tok == '('  = TokLParen : getTokens list
    | tok == ')'  = TokRParen : getTokens list
    | isDigit tok = number tok list
    | isSpace tok = getTokens list
    | otherwise = [TokErr ("Cannot getTokens " ++ [tok] )]


isDouble :: Char -> Bool
isDouble c = c `elem` "0123456789."

number :: Char -> String -> [Token]
number c cs =
    let (digs, cs') = span isDouble cs in
    TokNum (read (c : digs) :: Double) : getTokens cs'

      ---- buildTreer ----
      {-
          Expression <- Term [+-] Expression
                      | Term
          Term       <- Factor [*/^] Term
                      | Factor
          Factor     <- Number
                      | '(' Expression ')'
                      | [+-] Factor
      -}

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | ErrorNode String
    deriving Show

tokenHead :: [Token] -> Token
tokenHead [] = TokEnd
tokenHead (t:_) = t

tokenTail :: [Token] -> [Token]
tokenTail [] = []
tokenTail (_:ts) = ts

expression :: [Token] -> (Tree, [Token])
expression toks =
    let (termTree, toks') = term toks -- une expression commence par un terme
    in
      case tokenHead toks' of
          -- Term [+-] Expression => si le token suivant est un operator il est suivi pas une expression
          (TokOp op) | op `elem`[Plus, Minus] ->
            let (exTree, toks'') = expression (tokenTail toks')
            in (SumNode op termTree exTree, toks'')
          _ -> (termTree, toks')


term :: [Token] -> (Tree, [Token])
term toks =
    let (facTree, toks') = factor toks -- un term commence par un factor
    in
      case tokenHead toks' of
          -- Factor [*/] Term => si il y un operator  alors il y un terme
          (TokOp op) | op `elem` [Times, Div, Power] ->
            let (termTree, toks'') = term (tokenTail toks')
            in (ProdNode op facTree termTree, toks'')
          _ -> (facTree, toks') -- Term => sinon c'est un factor tout seul


factor :: [Token] -> (Tree, [Token])
factor toks =
 case tokenHead toks of
    (TokNum x) ->
          case tokenHead (tokenTail toks) of
              (TokNum _) -> (ErrorNode "Missing operand next to number", tokenTail toks)
              TokErr str -> (ErrorNode $ "Missing error unknow token " ++ str, tokenTail toks)
              _ -> (NumNode x, tokenTail toks) -- si c'est un nombre on creer un node Number
    (TokOp op) | op `elem` [Plus, Minus] -> -- si c'est un operator +- alors il y à un autre factor
          let (facTree, toks') = factor (tokenTail toks)
          in (UnaryNode op facTree, toks')
    TokLParen -> -- si il ya une parenthese auvrante ont recupère l'arbre de l'expresion à l'intèrieur
                 --et on regarde si le dernier token est bien une parenthèse fermante
       let (expTree, toks') = expression (tokenTail toks)
       in
          if tokenHead toks' /= TokRParen
          then (ErrorNode "Missing right parenthesis", tokenTail toks)
          else (expTree, tokenTail toks')
    _ -> (ErrorNode ("buildTree error on token: " ++ show toks), tokenTail toks)



buildTree :: [Token] -> Tree
buildTree toks = let (tree, toks') = expression toks
              in tree
---- evaluator ----

evaluate :: Tree -> Either String Double
evaluate (SumNode op left right) =
    case evaluate left of
    Left msg -> Left msg
    Right lft ->
        case evaluate right of
        Left msg -> Left msg
        Right rgt ->
            case op of
            Plus  -> Right (lft + rgt)
            Minus -> Right (lft - rgt)
            Err msg -> Left msg
            _       -> Left $ "Error wrong operator: " ++ show op

evaluate (ProdNode op left right) =
    case evaluate left of
    Left msg -> Left msg
    Right lft ->
        case evaluate right of
        Left msg -> Left msg
        Right rgt ->
            case op of
            Times -> Right (lft * rgt)
            Div   -> if rgt == 0
                    then Left "Error dividing by 0"
                    else Right (lft / rgt)
            Power -> Right (lft ** rgt)
            Err msg -> Left msg
            _       -> Left $ "Error wrong operator: " ++ show op

evaluate (UnaryNode op tree) =
    case evaluate tree of
    Left msg -> Left msg
    Right x ->
        case op of
        Plus  -> Right x
        Minus -> Right (-x)
        Err msg -> Left msg
        _       -> Left $ "Error wrong operator: " ++ show op

evaluate (NumNode x) =  Right x
evaluate (ErrorNode str) = Left str

roundResult :: Double -> Double
roundResult nb = (/100) $ fromIntegral $ round (nb * 100)
