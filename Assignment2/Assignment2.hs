{-# LANGUAGE TemplateHaskell #-}
module Assignment2 where
-- Julia Bäcklund (ju5360ba-s)
-- Erik Dahlberg (er8251da-s)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
type AlignmentType = (String, String)

similarityScore :: String -> String -> Int
similarityScore remainder [] = scoreSpace * length remainder
similarityScore [] remainder = scoreSpace * length remainder
similarityScore (x:xs) (y:ys) = max (similarityScore xs ys + score x y)
                                   (max (similarityScore xs (y:ys) + score x '-')
                                        (similarityScore (x:xs) ys + score '-' y))

newSimilarityScore :: String -> String -> Int
newSimilarityScore xs ys = nssLen (length xs) (length ys)
    where
        nssLen i j = nssTable!!i!!j
        nssTable = [[ nssEntry i j | j<-[0..]] | i<-[0..] ]

        nssEntry :: Int -> Int -> Int
        nssEntry 0 j = scoreSpace * j
        nssEntry i 0 = scoreSpace * i
        nssEntry i j = max (nssLen (i-1) (j-1) + score x y)
                                   (max (nssLen (i-1) j + score x '-')
                                        (nssLen i (j-1) + score '-' y))
            where
                x = xs!!(i-1)
                y = ys!!(j-1)


score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y = scoreMatch
    | x /= y = scoreMismatch

-- attachHeads takes two input arguments of type a and a list with pairs of type a. 
-- The function takes the two input arguments and place them at the head of every corresponding list pairs in "aList"
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- takes a function with domain a->b and a list with type a and returns the maximum list of values in the original list, based on the function provided
maximaBy :: Ord b => (a->b) -> [a] -> [a]
maximaBy f ys = [y | y <- ys, f y == maximumValues]
    where maximumValues = maximum (map f ys)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [("","")]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs "") -- Injicera space
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments "" ys) -- Injicera space
optAlignments (x:xs) (y:ys) = maximaBy (uncurry maxScore) (attachHeads x '-' (optAlignments xs (y:ys)) ++ attachHeads '-' y (optAlignments (x:xs) ys) ++ attachHeads x y (optAlignments xs ys))

maxScore x y = sum $ zipWith score x y

newOptAlignments :: String -> String -> [AlignmentType]
newOptAlignments xs ys = map
                          (apply reverse)
                          (snd $ noaLen (length xs) (length ys))
-- newOptAlignments xs ys = noaLen (length xs) (length ys)
    where
        apply f (a,b) = (f a, f b)

        noaLen i j = noaTable!!i!!j
        noaTable = [[ noaEntry i j | j<-[0..]] | i<-[0..] ]

        noaEntry :: Int -> Int -> (Int, [AlignmentType])
        noaEntry 0 0 = (0, [("","")])
        noaEntry i 0 = (scoreSpace*i, attachHeads (xs!!(i-1)) '-' (snd $ noaLen (i-1) 0))
        noaEntry 0 j = (scoreSpace*j, attachHeads '-' (ys!!(j-1)) (snd $ noaLen 0 (j-1)))
        noaEntry i j = (fst (head max), concatMap snd max)
            where 
                (xScore, xEntry) = noaLen (i-1) j
                (yScore, yEntry) = noaLen i (j-1)
                (xyScore, xyEntry) = noaLen (i-1) (j-1)
                x = xs !! (i-1)
                y = ys !! (j-1)
                max = maximaBy fst [  (xyScore + score x y, attachHeads x y xyEntry), 
                                        (xScore + score x '-', attachHeads x '-' xEntry),
                                        (yScore + score '-' y, attachHeads '-' y yEntry)    ] 

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments x y = do
  let a = newOptAlignments x y
  let n = length a
  mapM_ (\(a, b) -> putStrLn $ "\n" ++ (unwords $ map return a) ++ "\n" ++ (unwords $ map return b)) a
  putStrLn ("n = " ++ show n)
