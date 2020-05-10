-- Basic gui components
module BasicGUI(Button(..)) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GUI

import Debug.Trace

-- Create a button widget
data Button = Button
    { btnLeftClick      :: Modifiers -> [WAppObject] -> [WAppObject]
    , btnLeftRelease    :: Modifiers -> [WAppObject] -> [WAppObject]
    , btnName           :: String 
    , btnPosition       :: (Int, Int)
    , btnSize           :: (Int, Int) }
instance IAppObject Button where
    name btn =
        btnName btn
instance IDrawable Button where -- TODO: Implement button drawing code
    drawObj btn state =
        color white $ 
            translate (fromIntegral $ fst $ position btn) (fromIntegral $ snd $ position btn) $
                rectangleSolid (fromIntegral $ fst $ size btn) (fromIntegral $ snd $ size btn)
    position btn =
        btnPosition btn
    size btn =
        btnSize btn
instance IInteractive Button where
    mouseDown btn state clickType mods (x, y)
        | inBtn =
            (btnLeftClick btn) mods state
        | otherwise =
            trace (show (x, y))
            state
        where
            (bx, by) = btnPosition btn
            (bw, bh) = btnSize btn
            (xi, yi) = (round x, round y)
            inBtn = (xi >= bx - (bw `div` 2)) && (xi <= bx + (bw `div` 2)) && (yi > by - (bh `div` 2)) && (yi <= by + (bh `div` 2))

    mouseUp btn state clickType mods (x, y)
        | inBtn =
            (btnLeftRelease btn) mods state
        | otherwise =
            state
        where
            (bx, by) = btnPosition btn
            (bw, bh) = btnSize btn
            (xi, yi) = (round x, round y)
            inBtn = (xi >= bx) && (xi <= bx + bw) && (yi > by) && (yi <= by + bh)

    updateObj btn state deltaTime = state
    keyDown btn state key mods = state
    keyUp btn state key mods = state
    mouseMove btn state pos = state
instance IContainer Button where
    children btn = []