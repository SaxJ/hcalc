{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Parser

run :: IO ()
run = do
  xs <- getLine
  putStrLn $ show $ eval xs
