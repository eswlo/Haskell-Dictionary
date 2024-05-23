-- a parser that process strings sent from the dict.org server's gcide database
-- see https://datatracker.ietf.org/doc/html/rfc2229 for string command code documentation
module GCideParser where

import System.IO
import Control.Exception (try, catch, SomeException)
import qualified Data.Map.Lazy as Map


-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

-- checkCode checks if the string from server contains codes that indicate definition info at the beginning
checkCode :: String -> Bool
checkCode (a:b:c:r)
    |((a=='1') && (b=='5') && (c=='1')) = True
    |((a=='1') && (b=='5') && (c=='0')) = True
    |((a=='2') && (b=='2') && (c=='0')) = True
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCode _ = False

-- checkCompleteCommand checks if the string from server is complete
checkCompleteCommand :: String -> Bool
checkCompleteCommand (a:b:c:r)
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCompleteCommand (a:r)
    |(a =='.') = True
    |otherwise = False
checkCompleteCommand lst = False

-- checkNoMatch checks if the command code signifies no match in the search
checkNoMatch :: String -> Bool
checkNoMatch (a:b:c:r)
    |((a=='5') && (b=='5') && (c=='2')) = True
    |((a=='5') && (b=='0') && (c=='0')) = True
    |((a=='5') && (b=='0') && (c=='1')) = True
    |otherwise = False
checkNoMatch _ = False


-- groupDefGC groups the definitions of searched word from GCide database 
groupDefGC :: [String] -> [[String]] -> [[String]]
groupDefGC [[]] llst = llst
groupDefGC [] llst = llst
groupDefGC (h:t) llst = do
    if (checkH h)
        then (groupDefGC t (llst++[[h]]))
        else (groupDefGC t (reverse (concat h (reverse llst))))
        where
            -- This checks for the rest of the definitions
            checkH (' ':' ':' ':' ':' ':' ':_) = False
            checkH (' ':' ':' ':a:'.':_)
                | isANumber a = True
                | otherwise = False

            checkH (' ':' ':' ':a:b:'.':_)
                | isANumber a && isANumber b = True
                | otherwise = False
            
            -- This checks for the first definition
            checkH (a:_)
                | a == ' ' = False
                | otherwise = True
            -- checkH (' ':' ':' ':_) = False

            -- checkH('_':_) = False
            -- checkH(_:_) = False
            -- checkH ('_':[]) = False

            isANumber c
                | '0' <= c && c <= '9' = True
                | otherwise = False
            concat e (h:t) = (h++[e]):t



-- storeResponses handles receiving and storing responses/strings sent from the server
storeResponses :: Handle -> [String] -> IO [String]
storeResponses handle lst = do
    response <- hGetLine handle
    if (checkNoMatch response) 
        then do 
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


-- checkCompleteCommandForEmptyingHandle specifically checks the command code of 250
-- for strings of the gcide database
checkCompleteCommandForEmptyingHandle :: String -> Bool
checkCompleteCommandForEmptyingHandle (a:b:c:r)
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCompleteCommandForEmptyingHandle lst = False


-- emptyHandle clears handle for the next lookup
emptyHandle :: Handle -> IO ()
emptyHandle handle = do
    response <- hGetLine handle
    if (checkCompleteCommandForEmptyingHandle response)
        then do 
            return ()
        else do 
            emptyHandle handle


-- Main for GCideParser
main :: Handle -> IO [[String]]
main handle = do 
    responses <- (storeResponses handle [""])
    if ((length responses) == 1 && (responses!!0) == "NoMatch")
        then do
            return [["NoMatch"]]
        else do
            let processedResponses = (drop 1 (reverse responses)) -- keep defs only
            let defLlst = (groupDefGC processedResponses [[[]]])
            -- empty the handle for future use
            emptyHandle handle
            return defLlst