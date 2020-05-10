module Main where

import App(start)
import GUI
import BasicGUI(Button(..))
import Data.List(deleteBy)

gui ::  [WAppObject]
gui =   [ WAppObject Button { btnLeftClick =    leftClickCallback
                            , btnLeftRelease =  \x -> \y -> y
                            , btnName =         "btn1"
                            , btnPosition =     (32, 32)
                            , btnSize =         (32, 32) }]

leftClickCallback mod state =
    removeObject "btn1" state   -- Delete self

main :: IO ()
main =
    start "Test GUI App" (640, 480) (300, 100) gui
