-- For requesting word suggestions/recommendations in scenarios where users may input words 
-- that are not correctly spelled.
module WordRecommListReq where

import qualified WordRecommListCtor
import Data.Char (toLower)
import qualified Data.Map.Lazy as Map

-- getRecommWords invokes WordRecommListCtor to receive a list of suggested words
-- and returns the appropriate one(s)
getRecommWords :: String -> (Map.Map String Integer) -> IO String
getRecommWords word wordFreqMap = do
    -- get list of recommended words from WordRecommListCtor module
    let lowerCaseWord = map toLower word
    recommList <- WordRecommListCtor.buildRecommList lowerCaseWord wordFreqMap
    if (((length recommList) == 1) && ((recommList!!0)== lowerCaseWord))
        then return word
        else do 
            if ((length recommList) /= 0)
                then do 
                    ret <- wordChoiceListener recommList word
                    return ret
                else return word

-- wordChoiceListener displays word choices and returns the one selected by users
wordChoiceListener :: [[Char]] -> String -> IO String
wordChoiceListener recommList word = do
    let size = (length recommList) - 1
    let msg = "You entered " ++ word ++ ". Did you mean: "
    putStrLn msg
    ret <- printWordChoices recommList word 1
    let index = ((read ret :: Int) - 1)
    if (elem index [0..size]) 
        then (return (recommList!!index))
        else return word


-- printWordChoices prints out the words in the list of recommendations/suggestions for users to choose
printWordChoices :: [[Char]] -> String -> Int -> IO String
printWordChoices recommList word i = do
    if (i <= (length recommList))
        then do
            let msg = (show i) ++ ": " ++ (recommList!!(i-1))
            putStrLn msg
            printWordChoices recommList word (i+1)
        else do
            let msg = "Please choose one from above, or press any other keys for the original word " ++ word ++ " you entered."
            putStrLn msg
            putStr "Your choice of word: "
            line <- getLine
            if (elem line (map show [1..(length recommList)]))
                then (return line)
                else (return (show ((length recommList) +1)))