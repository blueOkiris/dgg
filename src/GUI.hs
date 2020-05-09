--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module GUI where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), Modifiers(..), KeyState(..))

data Button = Button
    { bgImage   :: Picture }

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

class (IDrawable obj) => IContainer obj where
    children        :: obj -> [WDrawable]

{-
data AppState

-- Class that will set up all other draw able objects
-- A widget and a layout both will have an IGUIObject instance and a widget instance
class IGUIObject obj state where
    draw    :: obj -> state -> Picture
    update  :: obj -> state -> state

-- Class for widgets (like an interface)
class (IGUIObject widget state) => IWidget widget state where
    event       :: widget -> state -> Event -> state
    event widget appState (EventKey (MouseButton btn) Down mod (x, y)) =
        appState
    event widget appState (EventKey (MouseButton btn) Up mod (x, y)) =
        appState
    event widget appState (EventKey (Char key) Down mod _) =
        appState
    event widget appState (EventKey (Char key) Up mod _) =
        appState
    event widget appState (EventKey (SpecialKey key) Down mod _) =
        appState
    event widget appState (EventKey (SpecialKey key) Up mod _) =
        appState
    event widget appState (EventMotion (x, y)) =
        appState
    event widget appState _ =
        appState

    keyDown     :: widget -> state -> Key -> Modifiers -> state
    keyUp       :: widget -> state -> Key -> Modifiers -> state
    mouseDown   :: widget -> state -> MouseButton -> Modifiers -> (Float, Float) -> state
    mouseUp     :: widget -> state -> MouseButton -> Modifiers -> (Float, Float) -> state
    mouseMove   :: widget -> state -> (Float, Float) -> state
        
-- Class for layouts (like an interface)
-- Need a gui object wrapper to extract draw from it
data WGUIObject = forall a b. IGUIObject a b => WGUIObject a b
instance IGUIObject WGUIObject AppState where
    draw (WGUIObject obj state) appState =
        draw obj state
    update (WGUIObject obj state) appState =
        update obj state

class (IGUIObject layout state) => ILayout layout state where
    children    :: layout -> state -> [WGUIObject]

-- Basic widgets
instance IGUIObject Button AppState where
    draw button state =
        Blank
    update button state =
        state
instance IWidget Button AppState where
    keyDown button state _ _ =
        state
    keyUp button state _ _ =
        state
    mouseDown button state _ _ (x, y) =
        state
    mouseUp button state _ _ (x, y) =
        state
    mouseMove buton state (x, y) =
        state
-}
