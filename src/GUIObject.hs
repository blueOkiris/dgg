{-# LANGUAGE MultiParamTypeClasses #-}
module GUIObject where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), Modifiers(..), KeyState(..))

data Button = Button
    { bgImage   :: Picture }

data AppState = AppState
    { temp      :: Int }

-- Class that will set up all other draw able objects
-- A widget and a layout both will have an IGUIObject instance and a widget instance
class IGUIObject obj state where
    draw    :: obj -> state -> Picture
    update  :: obj -> state -> state

-- Class for widgets
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

