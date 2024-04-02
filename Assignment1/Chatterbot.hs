module Chatterbot where
-- Julia Bäcklund (ju5360ba-s)
-- Erik Dahlberg (er8251da-s)
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  r <- randomIO :: IO Float
  return $ rulesApply $ (map . map2) (id , pick r) brain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply trans wrds = k
  where
    Just k = orElse (transformationsApply "*" reflect trans wrds)  (Just [])

-- Lookup gives key value 
reflect :: Phrase -> Phrase
reflect = map $ try $ flip lookup reflections

-- reflect :: Phrase -> Phrase
-- reflect [] = []
-- reflect (w:ws)
--     | isNothing (lookup reflections w) = w: reflect ws
--     | otherwise = present (lookup reflections w) : reflect ws

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map $ map2 (words . map toLower, map words)




reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute y (x:xs) ls
    | y == x = ls ++ substitute y xs ls
    | otherwise = x:substitute y xs ls


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- match grundar sig på att hitta en sublista i [l:ls]. Vi vill vara försäkrade oss om att ett wildcard finns i x:xs och sedan testa för matchingar i ls. 
-- match grundar sig på att hitta en sublista i [l:ls]. Vi vill vara försäkrade oss om att ett wildcard finns i x:xs och sedan testa för matchingar i ls. 
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing
match y (x:xs) (l:ls)
    | y == x = orElse (singleWildcardMatch (x:xs) (l:ls)) (longerWildcardMatch (x:xs) (l:ls))
    | x == l = match y xs ls
    | otherwise = Nothing


-- 1. Kolla ifall wildcard är densamma som första i x
-- 2. Ifall de är samma testar vi resten av sekvenserna av i SingleWildcardMatch och LongerWildcardmatch
-- 3. Ifall de inte är samma finns två fall som behöver undersökas: a) elementen x och l är samma och sökningen ska fortsätta på nästa nivå b) sekvenserna är olika och kan därför inte matchas

-- checks if the two lists are the same given that x == wildcard
singleWildcardMatch:: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (x:xs) (l:ls)
    | isJust (match x xs ls) = Just [l]
    | otherwise = Nothing

longerWildcardMatch:: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (x:xs) (l:ls) = mmap (l:) (match x (x:xs) ls)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- -- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f inp (sve, fre) = mmap (substitute w fre . f) (match w sve inp) -- match ger namn -> substitute lägger in namnet

-- 1. Hitta match i input (namn i exemplet) match * (exempelmening) (input)
-- 2. Använd namnet för att substitute in i översättingen substitute * (översättning) (namn)

transformationsApply :: Eq a => a -> ([a] -> [a]) -> 
                        [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply _ _ _ [] = Nothing
transformationsApply w f ((sve, fre):i) inp = orElse (transformationApply w f inp (sve,fre)) (transformationsApply w f i inp)


