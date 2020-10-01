module Parser
  ()
where

import           Data.Char
import           Control.Monad
import           Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(res, [])] -> res
  [(_  , rs)] -> error "Parser could not finish"
  _           -> error "Wat happened :("

item :: Parser Char
item = Parser $ \s -> case s of
  []       -> []
  (c : cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser fun) = Parser (\s -> [ (f a, b) | (a, b) <- fun s ])

instance Applicative Parser where
  pure = return
  (Parser f1) <*> (Parser f2)
