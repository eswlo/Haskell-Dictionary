{-
References:
for network: 
https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html
https://wiki.haskell.org/Implement_a_chat_server
https://stackoverflow.com/questions/31279673/reading-from-a-socket-while-keeping-the-connection-open

for spelling correction/recommendation:
https://norvig.com/spell-correct.html
https://github.com/cbeav/symspell/blob/master/resources/frequencies.txt
-}

-- This module contains the main entry point into the program. Only high level function calls are invoked, 
-- and the rest is taken care of by modules such as SocketHelpers and WordRecommListReq, 
-- while the decorative print statements are taken care of by the Pretty module

module Socket (main) where 

import Pretty (prettyPrintGoodBye, prettyPrintAsk, prettyPrintAfterResponse, prettyPrintWelcome, prettyPrintWord)
import Network.Socket
import System.IO
import Control.Exception (catch, SomeException(SomeException))
import SocketHelpers
import qualified Control.Monad
import qualified Data.Map.Lazy as Map
import qualified WordRecommListReq 
import qualified WordRecommListCtor
import qualified WordNetParser
import qualified GCideParser
-- import qualified FoldocParser


-- | Main function
main :: IO ()
main = withSocketsDo $ do
    prettyPrintWelcome

    -- Establish connection and get handle
    (handle, database) <- handleConnection
  
    -- Create word-freq map
    wordFreqMap <- WordRecommListCtor.wordFreqMapCtor

    -- run the application w/o reestablishing connection to server
    run handle database wordFreqMap

    -- Close the application and connection
    prettyPrintGoodBye
    hClose handle
    return ()


-- Run the ductionary application
run :: Handle -> String -> (Map.Map String Integer)-> IO ()
run handle db wordFreqMap = do
    -- Enter the word to look up
    putStr "Enter the word to look up: "
    line <- getLine
    let newLine = fixdel line

    -- Get recommended word if comfrimed to be potentially misspelt
    word <- WordRecommListReq.getRecommWords newLine wordFreqMap

    -- Send a request to define a confimred word
    hPutStrLn handle $ "DEFINE " ++ db ++ " " ++ word
    hFlush handle

    llst <- if db == "wn" then WordNetParser.main handle
        else if db == "gcide" then GCideParser.main handle
        -- else if db == "foldoc" then FoldocParser.main handle
        else error "Invalid databases at [run llst assignment]"

    if ((length llst) == 1 && ((llst!!0)!!0) == "NoMatch")
        then do
            putStrLn "No Match. Please try again"
            run handle db wordFreqMap
        else do 
            prettyPrintWord word

            viewDef (drop 1 llst)

            prettyPrintAsk
            
            state <- stateListener
            putStrLn " "
            if (state == "s" || state == "S") 
                then do 
                    run handle db wordFreqMap
            else Control.Monad.when (state == "q" || state == "Q") $ do
                return ()