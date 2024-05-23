{-
The code below is a Haskell version of Norvig's spelling corrector
Ref:
https://norvig.com/spell-correct.html
https://github.com/cbeav/symspell/blob/master/resources/frequencies.txt
https://www.olivierverdier.com/posts/2015/02/07/Haskell-spelling/
-}
-- Purpose and Usage:
-- To recommend at most top 4 possible corrections for a misspelt word.
-- In the main application, check if an entered word exist in the frequencies.txt.
-- If so, don't run this recommender and proceed to search. 
-- If not found, run it and display at most 4 choices to users, including the origianl misspelt word.

module WordRecommListCtor where

import Data.Char (toLower)
import qualified Data.Map.Lazy as Map
import Data.List (nub)

-- #############################################################################################
-- ###  buildRecommList returns a recommended list of up to 4 words using the misspelt word  ###
-- #############################################################################################
buildRecommList :: String -> (Map.Map String Integer) -> IO [[Char]]
buildRecommList word wordFreqMap = do 
    -- wordFreqMap <- wordCounter
    if (Map.member word wordFreqMap)
        then return [word]
        else do
            ret <- recomm word wordFreqMap
            return ret



-- #########################################################################
-- ###  Codes below are called by buildRecommList to construct the list  ###
-- #########################################################################

-- editors
-- splits generates a list of pairs, with each pair consisting of a prefix and a suffix of the input string
splits :: [Char] -> [([Char], [Char])]
splits lst = zip (reverse (makeLst1 (map toLower lst))) (makeLst2 (map toLower lst))
    where
        makeLst1 [] = [[]]
        makeLst1 lst = lst:(makeLst1 (init lst))
        makeLst2 [] = [[]]
        makeLst2 (h:t) = (h:t):(makeLst2 t)

-- deletes generates a list of words by deleting exactly one character in the input string
deletes :: [Char] -> [[Char]]
deletes lst = [ x++t | (x, (h:t)) <- (splits lst), (h:t)/=[]]

-- transposes generates a list of words by transposing adjacent characters in the input string
transposes :: [Char] -> [[Char]]
transposes lst = [ x++(b:a:t) | (x, (a:b:t)) <- (splits lst), (length (a:b:t)) > 1]

-- replaces generates a list of words by replacing exactly one character in the input string
-- with letters from 'a' to 'z'
replaces :: [Char] -> [[Char]]
replaces lst = foldr (\x v -> if (length (fst x) > 0) then ([(init (fst x)) ++ [c]++ (snd x)| c <- ['a'..'z']]++v) else v) [[]] (splits lst)

-- inserts generates a list of words by inserting letters from 'a' to 'z' into each position 
-- of the input string
inserts :: [Char] -> [[Char]]
inserts lst = foldr (\x v -> if (length (fst x) >= 0) then ([(fst x) ++ [c]++ (snd x)| c <- ['a'..'z']]++v) else v) [[]] (splits lst)


-- edits1 generates a list of edited words that are all one edit away from input string
edits1 :: [Char] -> [[Char]]
edits1 lst = ((deletes lst)++(transposes lst)++(replaces lst)++(inserts lst))


-- edits2 generates a list of edited words that are all 2 edits away from input string
edits2 :: [Char] -> [[Char]]
edits2 lst = foldr (\x v -> (edits1 x)++v) [[]] (edits1 lst)

-- stringToInteger converts String to Integer
stringToInteger :: String -> Integer
stringToInteger str = read str


-- makeMap creates a map of (word, frequency) 
makeMap :: [String] -> (Map.Map String Integer)
makeMap [] = Map.empty
makeMap (k:v:xs) = Map.insert k (stringToInteger v) (makeMap xs)


-- wordFreqMapCtor reads file and constructs a map of (word, frequency)
wordFreqMapCtor :: IO (Map.Map String Integer)
wordFreqMapCtor = do
    file <- readFile "frequencies.txt"
    let content = words file
    -- putStrLn (show (length content))
    let wordFreqMap = makeMap content
    return wordFreqMap

-- convertMaybe converts Just a to a, and Nothing to 0
convertMaybe :: (Integral a) => Maybe a -> a
convertMaybe (Just a) = a
convertMaybe Nothing  = 0

-- prob calculates the Probability of a given input string/word
prob :: [Char] -> (Map.Map String Integer) -> Integer
prob w dict = ((convertMaybe v))
    where 
        v = Map.lookup w dict

-- known generates a list of words where a given word is included if found in frequencies.txt
known :: [[Char]] -> (Map.Map String Integer) -> [[Char]]
known lst dict = filter (\x -> if (Map.member x dict) then True else False) lst

-- candidates generates a list of possible choices for the potentially misspelt input string.
candidates :: [Char] -> (Map.Map String Integer) -> [[Char]]
candidates lst dict
    |(length (known (edits1 lst) dict) /= 0) = known (edits1 lst) dict
    |otherwise = known (edits2 lst) dict


-- replace replaces an old string with a new one in the original list of strings
replace :: [Char] -> [Char] -> [[Char]] -> [[Char]]
replace t r (x:xs)
    |(t==x) = r:xs
    |otherwise = x: (replace t r xs)

-- searchReplace finds what to replace
searchReplace :: Int -> Int -> Int -> [[Char]] -> [[Char]] -> [[Char]]
searchReplace i low high llst temp = do
    if (i<=high)
        then do 
            let target = (llst!!i)
            let replacement = (temp!!(i-low))
            let update = replace target replacement llst
            searchReplace (i+1) low high update temp
        else llst
 
-- recurM is a recursive merge helper that continuously divide the list into only a single element
recurM :: Int -> Int -> Int -> Int -> Int -> Int -> [[Char]] -> [[Char]] -> (Map.Map String Integer) -> IO [[Char]]
recurM a b k low mid high llst temp dict
    |(k <= high) = do
        if (a <= mid && (b > high || (prob (llst!!a) dict) < (prob (llst!!b) dict)))
            then do
                let newTemp = (llst!!a) : temp
                recurM (a+1) b (k+1) low mid high llst newTemp dict
            else do
                let newTemp = (llst!!b) : temp
                recurM a (b+1) (k+1) low mid high llst newTemp dict
    |otherwise = do
        let i = low
        return (searchReplace i low high llst temp)


-- merge merges the sorted 
merge :: [[Char]] -> Int -> Int -> Int -> [[Char]] -> (Map.Map String Integer) -> IO [[Char]] 
merge llst low mid high temp dict = do
    let a = low
    let b = mid + 1
    let k = low
    merged <- recurM a b k low mid high llst temp dict
    return merged

    
-- mSort sorts left and right of the given list of strings
mSort :: [[Char]] -> Int -> Int -> [[Char]] -> (Map.Map String Integer) -> IO [[Char]]
mSort llst low high temp dict = do
    if (low < high) 
        then do
            let mid = (low + high) `div` 2
            _ <- mSort llst low mid temp dict
            _ <- mSort llst (mid+1) high temp dict
            merged <- merge llst low mid high temp dict
            return merged
        else return []


-- mergeSort is merge sort
mergeSort :: [[Char]] -> (Map.Map String Integer) -> IO [[Char]] 
mergeSort llst dict = do
    ret <- mSort llst 0 ((length llst) -1) [[]] dict
    return ret


-- recomm recommends/suggests top 4 possible choices from the list generated based on the
-- potentially misspelled input word
recomm :: [Char] -> (Map.Map String Integer) -> IO [[Char]]
recomm word dict = do
    let candidatesLlst = candidates word dict
    ret <- mergeSort candidatesLlst dict
    return (take 4 (reverse (nub ret)))


-- #############################################################################
-- ###    Below is specifically for interacting directly with this module    ###
-- ###  Not part of the editor for generating the recommended list of words  ###
-- #############################################################################

-- printRes prints out the result of the generated list
printRes :: [String] -> IO ()
printRes [] = return ()
printRes (h:t) = do 
    if (length (h:t) /= 0)
        then do
            putStrLn h 
            printRes t 
        else return ()

-- play allow users to interact with the module and see how it works
play :: (Map.Map String Integer) -> IO ()
play dict = do
    putStrLn "Enter a word"
    line <- getLine
    -- putStrLn (show (length f))
    g <- recomm line dict
    let res = "you entered " ++ line++". Do you mean: "
    putStrLn res
    printRes g
    -- putStrLn (g!!3)
    play dict

-- Main for WordRecommListCtor
main :: IO ()
main = do 
    dict <- wordFreqMapCtor
    play dict
    -- putStrLn (show (Map.lookup "|not" a))
    -- let b = prob "not" a
    -- putStrLn (show b)
    -- let c = Map.toList a 
    -- putStr (show (snd (c!!8)))
    -- let d = known ["qwhiodw", "not"] a 
    -- putStrLn (show (length d))
    -- let e = candidates "aple" dict 
    -- putStrLn (show (length e))
    -- putStrLn (head e) 
    -- line <- getLine
    -- let f = candidates line dict 
    -- -- putStrLn (show (length f))
    -- g <- recomm f dict
    -- putStrLn "you entered aple. Do you mean: "
    -- putStrLn (g!!0)
    -- putStrLn (g!!1)
    -- putStrLn (g!!2)
    -- putStrLn (g!!3)
    -- putStrLn (head e) 
    
    

