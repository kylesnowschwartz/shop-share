module Main where

import qualified Server

main :: IO ()
main = Server.runServer Server.defaultConfig
