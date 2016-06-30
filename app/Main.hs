module Main where

import System.Environment (getArgs)

import Link.Server

main :: IO ()
main = do
  port <- fmap (read . head) getArgs
  runServer port

