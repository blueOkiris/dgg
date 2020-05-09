--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module GUI where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), Modifiers(..), KeyState(..))
import qualified Data.Map.Strict as Map

class IAppObject obj where
    name :: obj -> String
-- Create a wrapper so we can store it all in dictionaries
data WAppObject = forall a. (IAppObject a) => WAppObject a
instance IAppObject WAppObject where
    name (WAppObject obj) =
        name obj

class (IAppObject obj) => IDrawable obj where
    drawObj     :: obj -> state -> Picture
    position    :: obj -> state -> (Int, Int)
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
    position (WDrawable obj) state =
        position obj state

-- Interactive widgets like a button
class (IAppObject obj) => IInteractive obj where
    eventHandler    :: obj -> state -> Event -> state
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

    updateObj       :: obj -> state -> state
    keyDown         :: obj -> state -> Key -> Modifiers -> state
    keyUp           :: obj -> state -> Key -> Modifiers -> state
    mouseDown       :: obj -> state -> MouseButton -> Modifiers -> (Float, Float) -> state
    mouseUp         :: obj -> state -> MouseButton -> Modifiers -> (Float, Float) -> state
    mouseMove       :: obj -> state -> (Float, Float) -> state

-- Layout things
class (IDrawable obj) => IContainer obj where
    children        :: obj -> [WDrawable]

data AppState = Map String WAppObject
