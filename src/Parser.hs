module Parser (tokenise) where

import Data.Char

data Expr 
    = Unary Operator Expr
    | Binary Operator Expr Expr
    | Function FunctionExpr [Expr]
    | Simple SimpleValue

newtype Operator = Operator Expr
newtype FunctionExpr = FunctionExpr [Expr]
newtype SimpleValue = SimpleValue Int

type Token = String

allowedSymbols :: [Char]
allowedSymbols = ['*', '+', '-', '/', '^', '%', '!', '(', ')']

validChar :: Char -> Bool
validChar c = isAlphaNum c || elem c allowedSymbols

-- Make some whitespace around the operators
splitAboutOperators :: String -> String
splitAboutOperators s = foldr fun [] s
    where
        fun c cs = if elem c allowedSymbols then ' ':c:' ':cs else c:cs

normalise :: String -> String
normalise = filter validChar

tokenise :: String -> [Token]
tokenise = words . splitAboutOperators . normalise
