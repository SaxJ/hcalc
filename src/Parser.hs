module Parser () where

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

single :: (Char -> Bool) -> Parser Char
single p = do
    x <- charP
    if p x then return x else empty
