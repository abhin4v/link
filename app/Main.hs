module Main where

import System.Environment (getArgs)

import Link.Server

main :: IO ()
main = do
  port <- fmap (read . (!! 0)) getArgs
  runServer port

