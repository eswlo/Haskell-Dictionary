-- This module contains the definitions of prints/putStrLns used throughout the program. 
-- This is done to increase readability and decouple the different modules

module Pretty (prettyPrintWord, prettyPrintWelcome, prettyPrintAfterResponse,prettyPrintAsk, 
prettyPrintGoodBye, prettyPrintInvalidInput, prettyPrintPrintDefEnd, prettyPrintSwitchDef, 
prettyPrintSwitchDefListener) where

import System.Console.ANSI

--prettyPrintWelcome prints out welcome message
prettyPrintWelcome :: IO ()
prettyPrintWelcome = do
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
    putStrLn " "
    putStrLn "♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡"
    putStr "$"
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
    putStr "------------------HELLO!---------------------"
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
    putStrLn "$"
    putStr "♡" 
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
    putStr "----------Welcome to our Dictionary----------"
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red] 
    putStrLn "♡" 
    putStrLn "$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$"
    putStrLn " "
    setSGR [Reset]
    return ()

--prettyPrintWelcome prints out farewell message
prettyPrintGoodBye :: IO ()
prettyPrintGoodBye = do 
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
            putStrLn " "
            putStrLn "♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡"
            putStr "$"
            setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
            putStr "-------Thanks for using our Dictionary-------"
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
            putStrLn "$"
            putStr "♡"
            setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
            putStr "-----------------GoodBye!--------------------"
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
            putStrLn "♡"
            putStrLn "$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$♡$"
            putStrLn " "
            setSGR [Reset]

-- prettyPrintAsk prints out message that inquires what users intend to do after existing viewing defs
prettyPrintAsk :: IO ()
prettyPrintAsk = do
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Green]
    putStrLn " "
    putStr "What would you like to do? Press s for a new search, or press q to quit: "
    setSGR [Reset]

-- prettyPrintAfterResponse prints out the word that users intend to look up
prettyPrintAfterResponse :: String -> IO ()
prettyPrintAfterResponse word = do
    putStrLn " "
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity, SetItalicized True]
    putStrLn word
    setSGR [Reset]
    putStrLn " "

-- prettyPrintInvalidInput prints out message when users select invalid inputs
prettyPrintInvalidInput :: IO ()
prettyPrintInvalidInput = do
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]  -- Changing background color
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
            putStrLn "+        Invalid input. Please try again.           +"
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
            setSGR [Reset]

-- prettyPrintSwitchDef prints out message that inquires what users what users intend to do 
-- while viewing definitions
prettyPrintSwitchDef :: IO ()
prettyPrintSwitchDef = do 
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Green]
            putStrLn " "
            putStr "Press n for next or p for previous, or e to exit viewing: "
            setSGR [Reset]

-- prettyPrintPrintDefEnd prints out the ending message when users reach the end of definitions
prettyPrintPrintDefEnd :: String -> IO ()
prettyPrintPrintDefEnd h = do 
                setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Blue]  -- Changing background color
                putStrLn h
                setSGR [Reset]

-- prettyPrintSwitchDefListener prints out message when users provide invalid inputs 
-- while viewing definitions        
prettyPrintSwitchDefListener :: IO () 
prettyPrintSwitchDefListener = do
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]  -- Changing background color
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
            putStrLn "+ Invalid input. Press n for next or p for previous, or e to exit viewing +"
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
            setSGR [Reset]

-- prettyPrintWord prints out the word that users intend to look up
prettyPrintWord :: String -> IO ()
prettyPrintWord word = do 
    putStrLn " "
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity, SetItalicized True]
    putStrLn word 
    setSGR [Reset]
    putStrLn " "

