module Nine where

  import Data.Bool
  import Data.Char
  import Data.List

  eftAny :: (Ord a, Enum a) => a -> a -> [a]
  eftAny left right = go [] left right
    where go  list    l r
            | l == r = list ++ [l]
            | r <= l = []
            | otherwise = go (list ++ [l]) lSucc r
                where lSucc = succ l

  eftOrd :: Ordering -> Ordering -> [Ordering]
  eftOrd = eftAny

  eftInt :: Int -> Int -> [Int]
  eftInt = eftAny

  eftChar :: Char -> Char -> [Char]
  eftChar = eftAny

  isSameChar :: Char -> Char -> Bool
  isSameChar compTo compChar = compTo == compChar


  breakLineBy :: Char -> String -> [String]
  breakLineBy breakChar str = go str []
      where go currentStr list
              | length currentStr == 0 = list
              | otherwise = go (dropWhile isCharNotPressent withoutEmptyStartStr) (list ++ [(takeWhile isCharNotPressent withoutEmptyStartStr)])
                              where withoutEmptyStartStr = dropWhile isCharPressent currentStr
                                    isCharPressent = isSameChar breakChar
                                    isCharNotPressent = not . isCharPressent

  myWords :: String -> [String]
  myWords = (breakLineBy ' ')

  firstSen = "Tyger Tyger, burning bright\n"
  secondSen = "In the forests of the night\n"
  thirdSen = "What immortal hand or eye\n"
  fourthSen = "Could frame thy fearful symmetry?"
  sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen


  myLines :: String -> [String]
  myLines = (breakLineBy '\n')

  shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

  main :: IO ()
  main =
    print $ "Are they equal? "
      ++ show (myLines sentences == shouldEqual)

  mySqr = [x^2 | x <- [1..5]]
  myCube = [y^3 | y <- [1..5]]

  getTuple f s = [(x, y) | x <- f, y <- s, x < 50, y < 50]

  first = length $ getTuple mySqr myCube

  foldBool = map (\x -> bool (x) (-x) (x == 3)) [1..10]

  multBy3 = filter (\x -> (mod x 3) == 0) [1..30]

  lengthOfMultBy3 = length multBy3

  myFilter line = filter (\x -> not (elem x articles)) (words line)
    where
      articles = ["the", "a", "an"]
      l = words line

  myZip = myZipWith (\x -> \y -> (x, y))

  myZipWith _ _ [] = []
  myZipWith _ [] _ = []
  myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

  filterLower str = filter isUpper str

  capitalizeFirstChar :: String -> String
  capitalizeFirstChar (c:str) = (toUpper c) : str

  capitalizeWord :: String -> String
  capitalizeWord (c:[]) = [toUpper c]
  capitalizeWord (c:str) = (toUpper c) : (capitalizeWord str)

  capitalizeFirstCharAndReturn :: String -> Char
  capitalizeFirstCharAndReturn = toUpper . head

  myOr [] = False
  myOr (b:bs) = b || myOr bs

  myAny _ [] = False
  myAny f (x: xs) = f x || myAny f xs

  myElem x xs = myAny f xs
      where f = \y -> x == y

  myReverse :: [a] -> [a]
  myReverse [] = []
  myReverse xs = (last xs) : (myReverse (init xs))

  squish [] = []
  squish (x:xs) = x ++ squish xs

  squishMap _ [] = []
  squishMap f (x:xs) = f x ++ squishMap f xs

  squishAgain = squishMap id

  compareMaxF o x y = if o == GT then x else y

  compareMinF o x y = if o == LT then x else y

  myCompareBy c f xs = go xs (xs !! 0)
    where go currentList currentValue
            | length currentList == 0 = currentValue
            | otherwise = go newList (c (f currentValue x) currentValue x)
                              where
                                x = (currentList !! 0)
                                newList = tail currentList

  myMaximumBy = myCompareBy compareMaxF

  myMinimumBy = myCompareBy compareMinF

  myMaximum :: (Ord a) => [a] -> a
  myMaximum = myMaximumBy compare

  myMinimum :: (Ord a) => [a] -> a
  myMinimum = myMinimumBy compare
