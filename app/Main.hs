module Main where

import System.Environment (getArgs)

import Link.Server

main :: IO ()
main = do
  port <- fmap (read . head) getArgs
  runServer "127.0.0.1" port

