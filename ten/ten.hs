module Ten where

    import Data.Time

    -- foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5] == foldl (*) 1 [1..5]

    -- foldl (flip (*)) 1 [1..3]
    -- (((1 * 1) * 2) * 3) without flip
    -- (3 * (2 * (1 * 1))) with flip = foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3 []

    -- 3 - c foldr, but not foldl associates to the right

    -- 4 - a reduce structure

    -- 5 - 
        -- a) foldr (++) [] ["woot", "WOOT", "woot"]
        -- b) foldr max 'a' "fear is the little death"
        -- c) foldr (&&) True [False, True]
        -- d) foldr (||) False [False, True]
        -- e) foldr ((++) . show) "" [1..5]
        -- f) foldr const 0 [1..5]
        -- g) foldl const 0 "tacos"
        -- h) foldr (flip const) 0 "burritos"
        -- i) foldr (flip const) 'z' [1..5]

    data DatabaseItem = DbString String
                        | DbNumber Integer
                        | DbDate UTCTime
                        deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase =
        [
            DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123)),
            DbNumber 9001,
            DbString "Hello, world!",
            DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123)),
            DbNumber 534
        ]


    isDbDate :: DatabaseItem -> Bool
    isDbDate (DbDate _) = True
    isDbDate _ = False

    getUTCTimeFromDbDate (DbDate t) = t

    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate l = map getUTCTimeFromDbDate $ filter isDbDate l

    isDbNumber :: DatabaseItem -> Bool
    isDbNumber (DbNumber _) = True
    isDbNumber _ = False

    getIntFromDbNumber (DbNumber i) = i

    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber l = map getIntFromDbNumber $ filter isDbNumber l

    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent l = foldr (\a b -> if b > a then b else a) init u
        where
            u = filterDbDate l
            init = u !! 0

    sumDb :: [DatabaseItem] -> Integer
    sumDb l = foldl (+) 0 u
            where u = filterDbNumber l

    avgDb :: [DatabaseItem] -> Double
    avgDb l = sum / count
        where
            sum = fromIntegral $ sumDb l
            count = fromIntegral $ length $ filterDbNumber l

    fibs = 1 : scanl (+) 1 fibs

    fibs20 = take 20 fibs

    fibsLess100 = takeWhile (\x -> x < 100) fibs

    stops = "pbtdkg"
    vowels = "aeiou"

    stopsVowels3 = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

    stopsVowels3StartsWithP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

    -- calculate average length of words of line
    seekritFunc :: String -> Int
    seekritFunc x =
        div (sum (map length (words x)))
        (length (words x))

    seekritFuncPrec :: Fractional a => String -> a
    seekritFuncPrec x =
        (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

    myOr :: [Bool] -> Bool
    myOr l = foldr orF False l
        where orF = (\acc v -> if acc == True then True else v)

    myOrPF :: [Bool] -> Bool
    myOrPF = foldr orF False
        where orF = (\acc v -> if acc == True then True else v)

    myOrPF2 :: [Bool] -> Bool
    myOrPF2 = foldr (||) False

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f l = myOrPF2 bl
        where bl = map f l

    myElem :: Eq a => a -> [a] -> Bool
    myElem v = myAny (\x -> x == v)

    myReverse :: [a] -> [a]
    myReverse = foldl (\acc v -> v : acc) []

    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr ((:) . f) []

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = foldr (\v acc -> if f v then v : acc else acc) []

    squish :: [[a]] -> [a]
    squish = foldr (++) []

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f = foldr ((++) . f) []

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f l = foldl (\v acc -> if (f v acc) == GT then v else acc) (l !! 0) l

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f l = foldl (\v acc -> if (f v acc) == GT then acc else v) (l !! 0) l
