module FoldocParser where

import System.IO
import Control.Exception (try, catch, SomeException)
import System.Console.ANSI
import qualified Data.Map.Lazy as Map


-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

checkCode :: String -> Bool
checkCode (a:b:c:r)
    |((a=='1') && (b=='5') && (c=='1')) = True
    |((a=='1') && (b=='5') && (c=='0')) = True
    |((a=='2') && (b=='2') && (c=='0')) = True
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCode _ = False

checkCompleteCommand :: String -> Bool
-- checkCompleteCommand (a:b:c:r)
--     |((a=='2') && (b=='5') && (c=='0')) = True
--     |otherwise = False
checkCompleteCommand (a:r)
    |(a =='.') = True
    |otherwise = False

checkNoMatch :: String -> Bool
checkNoMatch (a:b:c:r)
    |((a=='5') && (b=='5') && (c=='2')) = True
    |((a=='5') && (b=='0') && (c=='0')) = True
    |((a=='5') && (b=='0') && (c=='1')) = True
    |otherwise = False
checkNoMatch _ = False


-- Group the definitions of searched word from Foldoc database 
groupDefFD :: [String] -> [[String]] -> [[String]]
groupDefFD [[]] llst = llst
groupDefFD [] llst = llst
groupDefFD (h:t) llst = do
    if (checkH h)
        then (groupDefFD t (llst++[[h]]))
        else (groupDefFD t (reverse (concat h (reverse llst))))
        where
            -- This checks for the rest of the definitions
            checkH (' ':' ':' ':'<':_) = True
            checkH (_:_) = False

            concat e (h:t) = (h++[e]):t



-- Define a function to handle receiving and storing responses from WordNet
storeResponses :: Handle -> [String] -> IO [String]
storeResponses handle lst = do
    response <- hGetLine handle
    -- putStrLn response
    if (checkNoMatch response) 
        then do 
            -- Test_Dict.main
            return ["NoMatch"]
        else do 
            if (checkCompleteCommand response) 
                then return (("End of definitions"++response):lst)
                else do
                    if (checkCode response)
                        then do 
                            (storeResponses handle lst)
                        else do 
                            (storeResponses handle (response:lst))

main :: Handle -> IO [[String]]
main handle = do 
    responses <- (storeResponses handle [""])
    if ((length responses) == 1 && (responses!!0) == "NoMatch")
        then do
            return [["NoMatch"]]
        else do
            let processedResponses = (drop 1 (reverse responses)) -- keep defs only
            -- putStrLn "processedResponses"
            -- putStrLn (processedResponses!!0)
            let defLlst = (groupDefFD processedResponses [[[]]])
            return defLlst