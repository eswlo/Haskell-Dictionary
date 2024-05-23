{-
References:
for network: 
https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html
https://wiki.haskell.org/Implement_a_chat_server
https://stackoverflow.com/questions/31279673/reading-from-a-socket-while-keeping-the-connection-open
-}

-- The module contains the list of helper functions used in the functionality of the Socket 

module SocketHelpers (handleConnection, databaseListener, fixdel, viewDef, stateListener) where

import Network.Socket
import qualified WordRecommListReq
import System.IO
import qualified Control.Monad
import Pretty (prettyPrintInvalidInput, prettyPrintSwitchDefListener, prettyPrintPrintDefEnd, prettyPrintSwitchDef)


-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

-- Gets input from user to determine of program should continue or exit
stateListener :: IO String
stateListener = do
    state <- getLine
    putStrLn ""
    let newState = fixdel state
    if (newState == "s") || (newState == "S") || (newState == "q") || (newState == "Q")
        then return newState
        else do
            putStrLn ""
            putStr "Invalid input. Please press either s for a new search, or q for terminating the application: "
            stateListener

-- Obtains the input from user to determine if they want the next/prev definiton, or exit the viewing
switchDefListener :: IO String
switchDefListener = do
    line <- getLine
    let newLine = fixdel line
    if newLine == "n" || newLine == "N" || newLine == "p" || newLine == "P" || newLine == "e" || newLine == "E"
        then return newLine
        else do
            prettyPrintSwitchDefListener
            switchDefListener

-- Prints a definition
printDef :: [String] -> IO ()
printDef (h:t) = if ((length (h:t))/=1)
    then do
        putStrLn h
        printDef t
    else do
        if (checkDefEnding h)
            then do
                prettyPrintPrintDefEnd h
                return ()
            else do
                putStrLn h
                return ()
                where
                    checkDefEnding ('E':'n':'d':' ':'o':'f':r) = True
                    checkDefEnding _ = False


-- Switches a defintion to something else
switchDef :: String -> Int -> [[String]] -> IO ()
switchDef ins index llst = do
    let lng = length llst
        formatNP = do
            prettyPrintSwitchDef
    if ins == "d"
        then do
            putStrLn " "
            printDef (head llst)
            putStrLn " "
            formatNP
            newIns <- switchDefListener
            switchDef newIns index llst
    else if ins == "n" || ins == "N"
        then do
            let newIndex = index + 1
            putStrLn " "
            printDef (llst!!mod newIndex lng)
            putStrLn " "
            formatNP
            newIns <- switchDefListener
            switchDef newIns (mod newIndex lng) llst
    else Control.Monad.when (ins == "p" || ins == "P") $ do
            let newIndex = index - 1
            putStrLn " "
            printDef (llst!!(mod newIndex lng))
            putStrLn " "
            formatNP
            newIns <- switchDefListener
            switchDef newIns (mod newIndex lng) llst


-- Initiates the viewing of definitions
viewDef :: [[String]] -> IO ()
viewDef llst = if ((length llst)==0)
    then return ()
    else do
        switchDef "d" 0 llst

-- Obtains the input from user to determine which dictionary/database to use
databaseListener :: IO String
databaseListener = do
    putStrLn "Please select the dictionary you'd like to use by pressing the corresponding number"
    putStrLn "1. WordNet"
    putStrLn "2. The Collaborative International Dictionary of English"
    -- putStrLn "3. Computer Dictionary Explanations"
    putStr "Your choice of dictionary: "
    line <- getLine
    let choice = fixdel line
    if (choice == "1")
        then return "wn"
    else if (choice == "2")
        then return "gcide"
    -- else if (choice == "3")
    --     then return "foldoc"
        else do
            prettyPrintInvalidInput
            databaseListener


-- Establishes connection 
handleConnection :: IO (Handle, String)
handleConnection = do
    -- Initialize connection parameters
    let dictHost = "dict.org"
        dictPort = "2628"

    -- Setup: choose database
    db <- databaseListener

    -- Connect to the DICT server
    addrInfos <- getAddrInfo Nothing (Just dictHost) (Just dictPort)
    let serverAddr = head addrInfos

    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    handle <- socketToHandle sock ReadWriteMode
    return (handle, db)

