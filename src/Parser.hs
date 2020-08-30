module Parser where

import Data.Char

data Expr 
    = Unary Operator Expr
    | Binary Operator Expr Expr
    | Function FunctionExpr [Expr]
    | Simple SimpleValue

newtype Operator = Operator Expr
newtype FunctionExpr = FunctionExpr [Expr]
newtype SimpleValue = SimpleValue Int

allowedOperators :: [Char]
allowedOperators = ['*', '+', '-', '/', '^', '%', '!', '(', ')']

validChar :: Char -> Bool
validChar c = isAlphaNum c || elem c allowedOperators

normalise :: String -> String
normalise = filter validChar
