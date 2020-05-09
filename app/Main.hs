module Main where

import App(start)

main :: IO ()
main =
    start "Test GUI App" (640, 480) (300, 100) []
