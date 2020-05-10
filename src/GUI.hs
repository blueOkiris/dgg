--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module GUI where

import Data.List(findIndices)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Interface.IO.Game

-- The generic wrapper object
data WAppObject = forall a. (IAppObject a, IDrawable a, IInteractive a, IContainer a) => WAppObject a

class IAppObject obj where
    name :: obj -> String
-- Create a wrapper so we can store it all in dictionaries
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
instance IDrawable WAppObject where
    drawObj (WAppObject obj) state =
        drawObj obj state
    position (WAppObject obj) =
        position obj
    size (WAppObject obj) =
        size obj

-- Interactive widgets like a button
class (IAppObject obj) => IInteractive obj where
    eventHandler    :: obj -> (Int, Int) -> [WAppObject] -> Event -> [WAppObject]
    eventHandler obj (screenWidth, screenHeight) state (EventKey (MouseButton btn) Down mod (x, y)) =
        mouseDown obj state btn mod (x + (fromIntegral screenWidth / 2), -(y - (fromIntegral screenHeight / 2)))
    eventHandler obj (screenWidth, screenHeight) state (EventKey (MouseButton btn) Up mod (x, y)) =
        mouseUp obj state btn mod (x + (fromIntegral screenWidth / 2), -(y - (fromIntegral screenHeight / 2)))
    eventHandler obj (screenWidth, screenHeight) state (EventKey (Char key) Down mod _) =
        keyDown obj state (Char key) mod
    eventHandler obj (screenWidth, screenHeight) state (EventKey (Char key) Up mod _) =
        keyUp obj state (Char key) mod
    eventHandler obj (screenWidth, screenHeight) state (EventKey (SpecialKey key) Down mod _) =
        keyDown obj state (SpecialKey key) mod
    eventHandler obj (screenWidth, screenHeight) state (EventKey (SpecialKey key) Up mod _) =
        keyUp obj state (SpecialKey key) mod
    eventHandler obj (screenWidth, screenHeight) state (EventMotion (x, y)) =
        mouseMove obj state (x + (fromIntegral screenWidth / 2), -(y - (fromIntegral screenHeight / 2)))
    eventHandler obj _ state _ =
        state

    updateObj       :: obj -> [WAppObject] -> Float -> [WAppObject]
    keyDown         :: obj -> [WAppObject] -> Key -> Modifiers -> [WAppObject]
    keyUp           :: obj -> [WAppObject] -> Key -> Modifiers -> [WAppObject]
    mouseDown       :: obj -> [WAppObject] -> MouseButton -> Modifiers -> (Float, Float) -> [WAppObject]
    mouseUp         :: obj -> [WAppObject] -> MouseButton -> Modifiers -> (Float, Float) -> [WAppObject]
    mouseMove       :: obj -> [WAppObject] -> (Float, Float) -> [WAppObject]
instance IInteractive WAppObject where
    eventHandler (WAppObject o) s e = eventHandler o s e
    updateObj (WAppObject o) s d = updateObj o s d
    keyDown (WAppObject o) k m = keyDown o k m
    keyUp (WAppObject o) k m = keyUp o k m
    mouseDown (WAppObject o) b m p = mouseDown o b m p
    mouseUp (WAppObject o) b m p = mouseUp o b m p
    mouseMove (WAppObject o) s p = mouseMove o s p

-- Layout things
class (IDrawable obj) => IContainer obj where
    children        :: obj -> [WDrawable]
instance IContainer WAppObject where
    children (WAppObject o) = children o

-- State manipulation
removeObject    :: String -> [WAppObject] -> [WAppObject]
replaceObject   :: String -> WAppObject -> [WAppObject] -> [WAppObject]
removeObject objName state
    | objInds == [] =
        state
    | otherwise =
        (fst $ splitAt (objInds !! 0) state) ++ (snd $ splitAt ((objInds !! 0) + 1) state)
    where
        objInds = (findIndices (\obj -> (name obj) == objName) state)
replaceObject objName newValue state
    | objInds == [] =
        state
    | otherwise =
        (fst $ splitAt (objInds !! 0) state) ++ [newValue] ++ (snd $ splitAt ((objInds !! 0) + 1) state)
    where
        objInds = (findIndices (\obj -> (name obj) == objName) state)

