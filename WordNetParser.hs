-- a parser that process strings sent from the dict.org server's WordNet (wn) database
-- see https://datatracker.ietf.org/doc/html/rfc2229 for string command code documentation
module WordNetParser where

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

-- isCodeForInitialInfo Checks if the string from server contains codes that indicate 
-- definition info at the beginning
isCodeForInitialInfo :: String -> Bool
isCodeForInitialInfo (a:b:c:r)
    |((a=='1') && (b=='5') && (c=='1')) = True
    |((a=='1') && (b=='5') && (c=='0')) = True
    |((a=='2') && (b=='2') && (c=='0')) = True
    -- |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
isCodeForInitialInfo _ = False

-- isCompleteCommand Checks if the string from server is complete
isCompleteCommand :: String -> Bool
isCompleteCommand (a:b:c:r)
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
isCompleteCommand (a:r)
    |(a =='.') = True
    |otherwise = False
isCompleteCommand lst = False

-- isNoMatch checks if the command code signifies no match in the search
isNoMatch :: String -> Bool
isNoMatch (a:b:c:r)
    |((a=='5') && (b=='5') && (c=='2')) = True
    |((a=='5') && (b=='0') && (c=='0')) = True
    |((a=='5') && (b=='0') && (c=='1')) = True
    |otherwise = False
isNoMatch _ = False


-- groupDefWN groups the definitions of searched word from WordNet database 
groupDefWN :: [String] -> [[String]] -> [[String]]
groupDefWN [[]] llst = llst
groupDefWN [] llst = llst
groupDefWN (h:t) llst = 
    if (checkH h)
        then (groupDefWN t (llst++[[h]]))
        else (groupDefWN t (reverse (concat h (reverse llst))))
        where
            checkH (' ':' ':' ':' ':_:' ':_:':':_) = True
            checkH (' ':' ':' ':' ':_:' ':_:_:':':_) = True
            checkH (' ':' ':' ':' ':_:_:_:' ':_:':':_) = True
            checkH (' ':' ':' ':' ':_:_:_:' ':_:_:':':_) = True
            checkH (' ':' ':' ':' ':_:':':_) = True
            checkH (' ':_) = False
            checkH ('E':_) = True
            checkH _ = True
            concat e (h:t) = (h++[e]):t


-- storeResponses handles receiving and storing responses/strings sent from the server
storeResponses :: Handle -> [String] -> IO [String]
storeResponses handle lst = do
    response <- hGetLine handle
    if (isNoMatch response) 
        then do 
            return ["NoMatch"]
        else do 
            if (isCompleteCommand response) 
                then return (("End of definitions"++response):lst)
                else do
                    if (isCodeForInitialInfo response)
                        then do 
                            (storeResponses handle lst)
                        else do 
                            (storeResponses handle (response:lst))


-- emptyHandle clears handle for the next lookup
emptyHandle :: Handle -> IO ()
emptyHandle handle = do
    response <- hGetLine handle
    if (isCompleteCommand response)
        then do 
            return ()
        else do 
            emptyHandle handle


-- Main for WordNetParser
main :: Handle -> IO [[String]]
main handle = do 
    responses <- (storeResponses handle [""])
    if ((length responses) == 1 && (responses!!0) == "NoMatch")
        then do
            return [["NoMatch"]]
        else do
            let processedResponses = (drop 2 (reverse responses)) -- keep defs only
            let defLlst = (groupDefWN processedResponses [[[]]])
            emptyHandle handle -- empty the handle for future use
            return defLlst