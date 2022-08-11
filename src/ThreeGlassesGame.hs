module ThreeGlassesGame where

import System.Random (randomRIO)
import Control.Monad.IO.Class

data Glasses = 
        BlueGlass  {glassState :: WinLost}
      | RedGlass   {glassState :: WinLost}
      | GreenGlass {glassState :: WinLost}
        deriving (Show, Eq)

data WinLost = 
         Bean 
       | Empty 
         deriving (Show, Eq)

-- Initial glasses status: all empties.
initialGlassState :: [Glasses]
initialGlassState = [(BlueGlass Empty), (GreenGlass Empty), (RedGlass Empty)]

-- Put the bean in one of three glasses.
placeBean :: (WinLost -> Glasses) -> WinLost -> [Glasses]
placeBean = \gs st -> [gs st]

-- Winning possibilities.
blueWin :: [Glasses]
blueWin = placeBean BlueGlass Bean ++ tail initialGlassState

greenWin :: [Glasses]
greenWin = placeBean BlueGlass Empty ++ placeBean GreenGlass Bean ++ drop 2 initialGlassState

redWin :: [Glasses]
redWin = init initialGlassState ++ placeBean RedGlass Bean

winnersFuncList :: [[Glasses]]
winnersFuncList = [blueWin, greenWin, redWin]


-- Random choose of winner from func: winnersFuncList
chooseRandomWinner :: MonadIO m => [b] -> m b
chooseRandomWinner winList = do
                     randomWinnerIs <- randomRIO (0, length winList - 1)
                     return $ winList !! randomWinnerIs


-- Get Random Winner
winnerIs :: MonadIO m => m [Glasses]
winnerIs = chooseRandomWinner winnersFuncList


-- Shuffle List Function
shuffle :: MonadIO m => [Glasses] -> m [Glasses]
shuffle list = case length list < 2 of
                True  -> return list
                False -> do 
                         i <- randomRIO (0, length(list) -1)
                         r <- shuffle (take i list ++ drop (i + 1) list)
                         return (list !! i : r)


-- Shuffle the Glasses in order to return 
-- them everytime in a different order.
shuffleWinningGlasses :: MonadIO m => m [Glasses]
shuffleWinningGlasses = do
              glasses <- winnerIs
              shuffle glasses
