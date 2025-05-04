module Main where

import ECM.Core

main :: IO ()
main = do
    factors <- factor 455839 2000 -- Example of usage
    print factors
