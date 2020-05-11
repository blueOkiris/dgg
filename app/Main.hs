module Main where

import App(start)
import GUI
import BasicGUI(Button(..))
import Data.List(deleteBy)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Juicy(loadJuicyPNG)
import Data.Maybe(fromMaybe)

import Debug.Trace

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
    do
        btn1ImageMaybe <- loadJuicyPNG "./images/test_btn.png"
        let btn1Image = fromMaybe (trace ("BLANK") Blank) btn1ImageMaybe

        let loadedImagesGUI = replaceObject "btn1" (WAppObject $ btn1 { btnBGImage = btn1Image }) gui

        start "Test GUI App" white (640, 480) (300, 100) loadedImagesGUI
