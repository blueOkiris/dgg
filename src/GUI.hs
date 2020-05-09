--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module GUI where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), Modifiers(..), KeyState(..))

class IAppObject obj where
    name :: obj -> String
-- Create a wrapper so we can store it all in dictionaries
data WAppObject = forall a. (IAppObject a) => WAppObject a
instance IAppObject WAppObject where
    name (WAppObject obj) =
        name obj

class (IAppObject obj) => IDrawable obj where
    drawObj     :: obj -> [WAppObject] -> Picture
    position    :: obj -> (Int, Int)
    size        :: obj -> (Int, Int)
-- Wrapper so container can store it
data WDrawable = forall a. (IDrawable a) => WDrawable a
-- Required in order to be an argument for a drawable
-- In a sense, IDrawable extends IAppObject
instance IAppObject WDrawable where
    name (WDrawable obj) =
        name obj
-- Now to actually make the Drawable wrapper
instance IDrawable WDrawable where
    drawObj (WDrawable obj) state =
        drawObj obj state
    position (WDrawable obj) =
        position obj
    size (WDrawable obj) =
        size obj

-- Interactive widgets like a button
class (IAppObject obj) => IInteractive obj where
    eventHandler    :: obj -> [WAppObject] -> Event -> [WAppObject]
    eventHandler obj state (EventKey (MouseButton btn) Down mod (x, y)) =
        mouseDown obj state btn mod (x, y)
    eventHandler obj state (EventKey (MouseButton btn) Up mod (x, y)) =
        mouseUp obj state btn mod (x, y)
    eventHandler obj state (EventKey (Char key) Down mod _) =
        keyDown obj state (Char key) mod
    eventHandler obj state (EventKey (Char key) Up mod _) =
        keyUp obj state (Char key) mod
    eventHandler obj state (EventKey (SpecialKey key) Down mod _) =
        keyDown obj state (SpecialKey key) mod
    eventHandler obj state (EventKey (SpecialKey key) Up mod _) =
        keyUp obj state (SpecialKey key) mod
    eventHandler obj state (EventMotion (x, y)) =
        mouseMove obj state (x, y)
    eventHandler obj state _ =
        state

    updateObj       :: obj -> [WAppObject] -> [WAppObject]
    keyDown         :: obj -> [WAppObject] -> Key -> Modifiers -> [WAppObject]
    keyUp           :: obj -> [WAppObject] -> Key -> Modifiers -> [WAppObject]
    mouseDown       :: obj -> [WAppObject] -> MouseButton -> Modifiers -> (Float, Float) -> [WAppObject]
    mouseUp         :: obj -> [WAppObject] -> MouseButton -> Modifiers -> (Float, Float) -> [WAppObject]
    mouseMove       :: obj -> [WAppObject] -> (Float, Float) -> [WAppObject]

-- Layout things
class (IDrawable obj) => IContainer obj where
    children        :: obj -> [WDrawable]

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
instance IDrawable Button where
    drawObj btn state =
        Blank                   -- TODO: Implement button drawing code
    position btn =
        btnPosition btn
    size btn =
        btnSize btn
instance IInteractive Button where
    mouseDown btn state clickType mods (x, y)
        | inBtn =
            (btnLeftClick btn) mods state
        | otherwise =
            state
        where
            (bx, by) = btnPosition btn
            (bw, bh) = btnSize btn
            (xi, yi) = (round x, round y)
            inBtn = (xi >= bx) && (xi <= bx + bw) && (yi > by) && (yi <= by + bh)

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

    updateObj btn state = state
    keyDown btn state key mods = state
    keyUp btn state key mods = state
    mouseMove btn state pos = state
