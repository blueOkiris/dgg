-- Point of this is to take app state and GUI framework and create an actual window
-- That window can then be created from here in the main function

module App(start) where

import Graphics.Gloss.Data.Display(Display(..))
import Graphics.Gloss.Data.Color(Color(..), black)
import Graphics.Gloss.Data.Picture(Picture(..))
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import System.Exit(exitSuccess)
import Debug.Trace(trace)

import GUI      -- Get everything from the GUI class. It's all needed

start       :: String -> Color -> (Int, Int) -> (Int, Int) -> [WAppObject] -> IO ()
appDraw     :: (Int, Int) -> [WAppObject] -> (IO Picture)
appEvent    :: (Int, Int) -> Int -> Event -> [WAppObject] -> (IO [WAppObject])
appUpdate   :: Int -> Float -> [WAppObject] -> (IO [WAppObject])

start title bgColor size position initalState =
    playIO window bgColor 30 initalState (appDraw size) (appEvent size 0) (appUpdate 0)
    where
        window = InWindow title size position

appDraw (width, height) objects =
    return $ applyViewPortToPicture 
                (viewPortInit   { viewPortTranslate = (-fromIntegral width / 2, fromIntegral height / 2)  }) $ 
                    Pictures $ map (\obj -> Scale 1 (-1) $ drawObj obj objects) objects

{-appEvent _ (EventKey (SpecialKey KeyF4) Down alt _) objects =
    do
        putStrLn "Exiting..."
        exitSuccess-}
appEvent (width, height) startInd event objects
    | length objects == 0 =
        --do
            --putStrLn "No objects"
            return objects
    | startInd == (length objects) - 1 =
        return newObjects
    | otherwise =
        appEvent (width, height) (startInd + 1) event newObjects
    where
        newObjects = eventHandler (objects !! startInd) (width, height) objects event

appUpdate startInd deltaTime objects
    | length objects == 0 =
        return objects
    | startInd == (length objects) - 1 =
        --do
            --putStrLn $ "There are " ++ (show $ length newObjects) ++ " objects."
            return $ newObjects
    | otherwise =
        appUpdate (startInd + 1) deltaTime newObjects
    where
        newObjects = updateObj (objects !! startInd) objects deltaTime
