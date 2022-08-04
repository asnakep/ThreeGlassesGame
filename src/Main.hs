module Main where

import ThreeGlassesGame

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import System.Console.Pretty
import System.Console.ANSI hiding (Red, Blue, Green, White)
import System.Exit
import System.IO


 -- processBlueGlass  
processBlueGlass :: IO ()
processBlueGlass = do
   list      <- shuffleWinningGlasses
   preresult <- return (BlueGlass Bean `elem` list)
   result    <- case preresult of
                       True  -> putStrLn (style Bold  $ "\nYou chose:\n")
                             >> putStrLn (color Blue  . style Bold $ "BlueGlass\n")
                             >> putStrLn (color Green . style Bold $ "You guessed it right! The Bean is inside the BlueGlass :)")
                       False -> putStrLn (style Bold  $ "\nYou chose:\n ")
                             >> putStrLn (color Blue  . style Bold $ "BlueGlass\n")
                             >> putStrLn (color Red   . style Bold $ "The Bean is not inside BlueGlass. :(")
   return result
   putStrLn ""
   putStrLn $ (style Bold $ show list)
   putStrLn ""


-- processGreenGlass
processGreenGlass :: IO ()
processGreenGlass = do
   list      <- shuffleWinningGlasses
   preresult <- return (GreenGlass Bean `elem` list)
   result    <- case preresult of
                       True  -> putStrLn (style Bold  $ "\nYou chose:\n")
                             >> putStrLn (color Green . style Bold $ "GreenGlass\n")
                             >> putStrLn (color Green . style Bold $ "You guessed it right! The Bean is inside the GreenGlass :)")
                       False -> putStrLn (style Bold  $ "\nYou chose:\n")
                             >> putStrLn (color Green . style Bold $ "GreenGlass\n")
                             >> putStrLn (color Red   . style Bold $ "The Bean is not inside GreenGlass. :(")
   return result
   putStrLn ""
   putStrLn $ (style Bold $ show list)
   putStrLn ""


-- processRedGlass
processRedGlass :: IO ()
processRedGlass = do
   list      <- shuffleWinningGlasses
   preresult <- return (RedGlass Bean `elem` list)
   result    <- case preresult of
                       True  -> putStrLn (style Bold  $ "\nYou chose:\n")
                             >> putStrLn (color Red   . style Bold $ "RedGlass\n")
                             >> putStrLn (color Green . style Bold $ "You guessed it right! The Bean is inside the RedGlass :)")
                       False -> putStrLn (style Bold  $ "\nYou chose:\n")
                             >> putStrLn (color Red   . style Bold $ "RedGlass\n")
                             >> putStrLn (color Red   . style Bold $ "The Bean is not inside RedGlass. :(")
   return result
   putStrLn ""
   putStrLn $ (style Bold $ show list)
   putStrLn ""


-- RunGame
numTries :: Int
numTries = 3

type Tries = Int
type RunGame a = StateT Tries IO a

runGame :: RunGame ()
runGame = do
        tries <- get
        case tries < 1 of
               True  -> do
                        lift $ putStrLn (style Bold  $ "\nGame Over\n ") >> continueOrExit
               False -> do
                        lift $ putStrLn ""
                        lift $ putStrLn $ (style Bold $ show tries) 
                                 ++ (style Bold  $ " tries left of ") 
                                 ++ (style Bold $ show numTries)
                        lift $ putStrLn (style Bold  $ "\nPlease choose your glass:\n") 
                        lift $ putStrLn $  
                                        (color Blue  . style Bold $ "BlueGlass " ) 
                                     ++ (color Green . style Bold $ "GreenGlass ") 
                                     ++ (color Red   . style Bold $ "RedGlass "  )
                        lift $ hSetEcho stdin False
                        choice <- lift $ getLine
                        case choice of
                           "BlueGlass"  -> lift $ processBlueGlass
                           "GreenGlass" -> lift $ processGreenGlass
                           "RedGlass"   -> lift $ processRedGlass
                           _            -> lift $ clearScreen >> restoreCursor
                        lift $ threadDelay 3000000 >> clearScreen >> restoreCursor
                        put (tries - 1)
                        runGame


-- playOrExit
playOrExit :: IO ()
playOrExit = do
      putStrLn (style Bold  $ "Enter P to Play or X to Exit.")
      hSetEcho stdin False
      choice <- getLine
      case choice of
         "P" -> clearScreen >> restoreCursor >> runStateT runGame numTries >> playOrExit
         "X" -> exitSuccess
         _   -> clearScreen >> restoreCursor >> playOrExit


-- continueOrExit
continueOrExit :: IO ()
continueOrExit = do
      putStrLn (style Bold  $ "Enter C to Continue or X to Exit.")
      hSetEcho stdin False
      choice <- getLine
      case choice of
         "C" -> clearScreen >> restoreCursor >> runStateT runGame numTries >> continueOrExit
         "X" -> exitSuccess
         _   -> clearScreen >> restoreCursor >> continueOrExit


-- gameIntro
gameIntro :: IO ()
gameIntro = do
   clearScreen >> restoreCursor
   putStrLn (style Bold  $              "\nThree Glasses Game:")
   putStrLn (color Green . style Bold $ "\nThe Gambler will put a Bean inside one of three upside down glasses \nand then will shuffle them very skillfully.")
   putStrLn (color Green . style Bold $ "\n Guess which glass contains the bean.\n")
   putStrLn (style Bold  $              " Input: BlueGlass, GreenGlass or RedGlass and let's see if you are lucky!\n")
   putStrLn (style Bold  $              " Different inputs will be rejected and you will loose a try.\n")
   putStrLn (style Bold  $              " Some info about the game:\n")
   putStrLn (style Bold  $              " https://en.wikipedia.org/wiki/Cups_and_balls\n")
   

--main function
main :: IO ()
main = do
    gameIntro >> playOrExit