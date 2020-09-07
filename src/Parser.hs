module Parser (eval) where

import Control.Applicative
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser x) input = x input

-- parses a character
charP :: Parser Char
charP  = Parser (\str -> case str of
                           [] -> []
                           (x:xs) -> [(x, xs)])

-- lets us apply a function in the parser context
instance Functor Parser where
    fmap fun parser = Parser (\input -> case parse parser input of
                                          [] -> []
                                          [(x, unconsumed)] -> [(fun x, unconsumed)])

-- lets us chain parsers
instance Applicative Parser where
    -- lifts a value x into the parser
    pure x = Parser (\input -> [(x, input)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = Parser (\input -> case parse pg input of
                                    [] -> []
                                    [(x, unc)] -> parse (fmap x px) unc)

instance Monad Parser where
    p >>= f = Parser (\input -> case parse p input of
                                  [] -> []
                                  [(val, output)] -> parse (f val) output)

instance Alternative Parser where
    empty = Parser (\_ -> [])
    p <|> q = Parser (\input -> case parse p input of
                                  [] -> parse q input
                                  result -> result)

cond :: (Char -> Bool) -> Parser Char
cond p = do
    x <- charP
    if p x then return x else empty

-- different parsers
digit :: Parser Char
digit = cond isDigit

lower :: Parser Char
lower = cond isLower

upper :: Parser Char
upper = cond isUpper

letter :: Parser Char
letter = cond isAlpha

alphanum :: Parser Char
alphanum = cond isAlphaNum

char :: Char -> Parser Char
char x = cond (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    many (cond isSpace)
    return ()

int :: Parser Int
int = do
    char '-'
    n <- nat
    return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

-- now parsers that ignore spacing
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

expr :: Parser Int
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (t + e)
        <|> return t

term :: Parser Int
term = do
    f <- factor
    do 
        symbol "*"
        t <- term
        return (f * t)
        <|> return f

factor :: Parser Int
factor = do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, out)] -> error ("Invalid input: " ++ out)
            [] -> error "Invalid input"
