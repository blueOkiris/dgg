-- Basic gui components
module BasicGUI(Button(..)) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GUI

import Debug.Trace

-- Create a button widget
data Button = Button
    { btnLeftClick              :: Modifiers -> [WAppObject] -> [WAppObject]
    , btnLeftRelease            :: Modifiers -> [WAppObject] -> [WAppObject]
    , btnName                   :: String 
    , btnPosition               :: (Int, Int)
    , btnSize                   :: (Int, Int) 
    , btnBorderColor            :: Color
    , btnBorderHighlightColor   :: Color
    , btnBorderWidth            :: Int
    , btnBGColor                :: Color
    , btnBGHighlightColor       :: Color
    , btnBGClickedColor         :: Color
    , btnBGImage                :: Picture 
    , btnIsHighlighted          :: Bool
    , btnIsClicked              :: Bool }
instance IAppObject Button where name btn = btnName btn
instance IContainer Button where children btn = []
instance IDrawable Button where -- TODO: Implement button drawing code
    drawObj btn state =
        Pictures    [ coloredBorder
                    , coloredFill 
                    , translate (fromIntegral $ fst $ position btn) (fromIntegral $ snd $ position btn) $ btnBGImage btn ]
        where
            border =        translate (fromIntegral $ fst $ position btn) (fromIntegral $ snd $ position btn) $
                                rectangleSolid (fromIntegral $ fst $ size btn) (fromIntegral $ snd $ size btn)
            coloredBorder = if btnIsHighlighted btn then color (btnBorderHighlightColor btn) border
                            else color (btnBorderColor btn) border
            fill =          translate (fromIntegral $ fst $ position btn) (fromIntegral $ snd $ position btn) $
                                rectangleSolid (fromIntegral $ (fst $ size btn) - (btnBorderWidth btn * 2)) (fromIntegral $ (snd $ size btn) - (btnBorderWidth btn * 2))
            coloredFill =   if btnIsClicked btn then color (btnBGClickedColor btn) fill
                            else if btnIsHighlighted btn then color (btnBGHighlightColor btn) fill
                            else color (btnBGColor btn) fill
    position btn = btnPosition btn
    size btn = btnSize btn
instance IInteractive Button where
    updateObj btn state deltaTime = state
    keyDown btn state key mods = state
    keyUp btn state key mods = state

    mouseMove btn state (x, y)
        | inBtn =                           replaceObject (btnName btn) (WAppObject $ btn { btnIsHighlighted = True }) state
        | btnIsHighlighted btn == True =    replaceObject (btnName btn) (WAppObject $ btn { btnIsHighlighted = False }) state
        | otherwise =                       state
        where
            (bx, by) = btnPosition btn
            (bw, bh) = btnSize btn
            (xi, yi) = (round x, round y)
            inBtn = (xi >= bx - (bw `div` 2)) && (xi <= bx + (bw `div` 2)) && (yi > by - (bh `div` 2)) && (yi <= by + (bh `div` 2))

    mouseDown btn state clickType mods (x, y)
        | inBtn =
            replaceObject (btnName btn) (WAppObject $ btn { btnIsClicked = True }) $
                (btnLeftClick btn) mods state
        | otherwise = state
        where
            (bx, by) = btnPosition btn
            (bw, bh) = btnSize btn
            (xi, yi) = (round x, round y)
            inBtn = (xi >= bx - (bw `div` 2)) && (xi <= bx + (bw `div` 2)) && (yi > by - (bh `div` 2)) && (yi <= by + (bh `div` 2))

    mouseUp btn state clickType mods (x, y)
        | btnIsClicked btn = 
            replaceObject (btnName btn) (WAppObject $ btn { btnIsClicked = False }) $
                (btnLeftRelease btn) mods state
        | otherwise = state