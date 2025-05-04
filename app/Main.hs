module Main where

import ECM.Core

main :: IO ()
main = do
    factors <- factor 780787 2000
    print factors
