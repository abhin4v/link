module Main where

import Link.Server
import System.Environment (getArgs)

main :: IO ()
main = do
  port <- fmap (read . (!! 0)) getArgs
  runServer port

