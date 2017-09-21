module Main where

import ParsingOutFile

main :: IO ()
main = do
  st <- readFile "char_gunstig.out"
  print $ fst $ last $ outFile st
