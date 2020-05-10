module Main where

import App(start)
import GUI
import BasicGUI(Button(..))
import Data.List(deleteBy)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))

btn1 :: Button
btn1 =  Button 
    { btnLeftClick =            \x -> \y -> y
    , btnLeftRelease =          \x -> \y -> y
    , btnName =                 "btn1"
    , btnPosition =             (64, 32)
    , btnSize =                 (64, 32) 
    , btnBorderColor =          black
    , btnBorderHighlightColor = black
    , btnBorderWidth =          2
    , btnBGColor =              blue
    , btnBGHighlightColor =     red
    , btnBGClickedColor =       green
    , btnBGImage =              Blank
    , btnIsHighlighted =        False
    , btnIsClicked =            False }

gui ::  [WAppObject]
gui =   [ WAppObject btn1 ]

main :: IO ()
main =
    start "Test GUI App" white (640, 480) (300, 100) gui
