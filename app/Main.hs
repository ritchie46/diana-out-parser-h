module Main where

import ParsingOutFile
import Types


-- main :: IO ()
main = do
  st <- readFile "char_gunstig.out"
  -- print $ fst $ last $ outFile st
  return $ outFile st
