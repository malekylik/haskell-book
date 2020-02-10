{-# LANGUAGE FlexibleInstances #-}

module Eleven where

    import Data.Int
    import Data.List
    import Data.Char

    -- Is Doggies a type constructor or a data constructor? - type constructor
    -- What is the kind of Doggies? - * -> *
    -- What is the kind of Doggies String? - Doggies String :: *
    -- What is the type of Husky 10? - Doggies Num
    -- What is the type of Husky (10 :: Integer)? - Doggies Integer
    -- What is the type of Mastiff "Scooby Doo"? - Doggies String
    -- Is DogueDeBordeaux a type constructor or a data constructor? - both with the same name
    -- What is the type of DogueDeBordeaux? - DogueDeBordeaux :: doge -> DogueDeBordeaux doge
    -- What is the type of DogueDeBordeaux "doggie!" - DogueDeBordeaux String

    data Price =
        Price Integer deriving (Eq, Show)

    data Manufacturer =
        Mini
        | Mazda
        | Tata
        deriving (Eq, Show)

    data Airline =
        PapuAir
        | CatapultsR'Us
        | TakeYourChancesUnited
        deriving (Eq, Show)

    data Vehicle = Car Manufacturer Price
        | Plane Airline Integer
        deriving (Eq, Show)

    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir 123

    -- What is the type of myCar? - Vehicle

    isCar :: Vehicle -> Bool
    isCar (Plane _ _) = False
    isCar (Car _ _) = True

    isPlane :: Vehicle -> Bool
    isPlane = not . isCar

    areCars :: [Vehicle] -> [Bool]
    areCars = map isCar

    getManu :: Vehicle -> Manufacturer
    getManu (Plane _ _) = undefined
    getManu (Car m _) = m

    data PugType = PugData -- cardinality = 1

    -- data Airline = -- cardinality = 3
    --         PapuAir
    --         | CatapultsR'Us
    --         | TakeYourChancesUnited

    -- Int16 cardinality = 32767 (maxBound) + |-32768| (minBound) + 1 (0) = 65536

    -- Int32 cardinality = 2147483647 (maxBound) + |-2147483648| (minBound) + 1 (0) = 4 294 967 296‬
    -- Int = Int64

    -- 8 = bits 256 = 2^8

    data Example = MakeExample deriving Show

    -- What is the type of data constructor MakeExample? - MakeExample :: Example
    -- What if you try :info on Example in GHCi? - Show

    data ExampleWithInt = MakeExampleWithInt Int deriving Show

    -- What has changed when you query MakeExample with :type in GHCi? - MakeExampleWithInt :: Int -> ExampleWithInt

    class TooMany a where
        tooMany :: a -> Bool

    instance TooMany Int where
        tooMany n = n > 42

    newtype Goats = Goats Int deriving Show

    instance TooMany Goats where
        tooMany (Goats n) = n > 43

    instance TooMany (Int, String) where
        tooMany (n, _) = n > 43

    instance TooMany (Int, Int) where
        tooMany (n, n1) = n + n1 > 43

    instance (Num a, TooMany a) => TooMany (a, a) where
        tooMany (a, b) = tooMany a && tooMany b

    data BigSmall = -- 2 + 2 = 4
        Big Bool -- 2
        | Small Bool -- 2
        deriving (Eq, Show)

    data NumberOrBool = -- 256 + 2 = 258
        Numba Int8 -- 256
        | BoolyBool Bool -- 2
        deriving (Eq, Show)

    data Fruit =
        Peach
        | Plum
        | Apple
        | Blackberry
        deriving (Eq, Show, Ord)

    data JamJars = -- 4 * 2^32
        Jam {
            getFruit :: Fruit,
            getJamCount :: Int
        }
        deriving (Eq, Show, Ord)

    row1 = Jam Peach 32
    row2 = Jam Plum 2
    row3 = Jam Apple 14
    row4 = Jam Blackberry 50
    row5 = Jam Plum 322
    row6 = Jam Apple 1234
    allJam = [row1, row2, row3, row4, row5, row6]
    sumJar = sum $ map getJamCount allJam
    mostRaw = foldr getLargest (head allJam) allJam
        where getLargest x y = if (getJamCount x) < (getJamCount y) then y else x

    compareKind (Jam k _) (Jam k' _) = compare k k'
    sortedAllJam = sortBy compareKind allJam
    allJamGroupedByFruitType = groupBy (\x y -> (getFruit x) == (getFruit y)) sortedAllJam

    data FlowerType = Gardenia
                    | Daisy
                    | Rose
                    | Lilac
                    deriving Show

    type Gardener = String

    data Garden =
        Garden Gardener FlowerType
        deriving Show

        -- String * (Gardenia + Daisy + Rose + Lilac)
        -- Garden Gardener Gardenia
        -- | Garden Gardener Daisy
        -- | Garden Gardener Rose
        -- | Garden Gardener Lilac

    data OperatingSystem =
        GnuPlusLinux
        | OpenBSDPlusNevermindJustBSDStill
        | Mac
        | Windows
        deriving (Eq, Show)

    data ProgrammingLanguage =
        Haskell
        | Agda
        | Idris
        | PureScript
        deriving (Eq, Show)

    data Programmer =
        Programmer { os :: OperatingSystem
        , lang :: ProgrammingLanguage }
        deriving (Eq, Show)

    allOperatingSystems :: [OperatingSystem]
    allOperatingSystems =
        [ GnuPlusLinux
        , OpenBSDPlusNevermindJustBSDStill
        , Mac
        , Windows
        ]

    allLanguages :: [ProgrammingLanguage]
    allLanguages = [Haskell, Agda, Idris, PureScript]

    allProgrammers :: [Programmer]
    allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

    data Quantum =
        Yes
        | No
        | Both
        deriving (Eq, Show)

    convert1 :: Quantum -> Bool
    convert1 Yes  = False
    convert1 No   = False
    convert1 Both = False

    convert2 Yes  = True
    convert2 No   = False
    convert2 Both = False

    convert3 Yes  = False
    convert3 No   = True
    convert3 Both = False

    convert4 Yes  = False
    convert4 No   = False
    convert4 Both = True

    convert5 Yes  = True
    convert5 No   = True
    convert5 Both = False

    convert6 Yes  = True
    convert6 No   = False
    convert6 Both = True

    convert7 Yes  = False
    convert7 No   = True
    convert7 Both = True

    convert8 Yes  = True
    convert8 No   = True
    convert8 Both = True

    data Quad = -- cardinality 4
        One
        | Two
        | Three
        | Four
        deriving (Eq, Show)

    -- how many different forms can this take?
    eQuad :: Either Quad Quad -- 4 (Left Quad) + 4 (Right Quad) = 8
    eQuad = undefined

    -- prodQuad :: (Quad, Quad) 4 * 4 = 16
    -- funcQuad :: Quad -> Quad 4^4 = 256
    -- prodTBool :: (Bool, Bool, Bool) 2 * 2 * 2 = 8
    -- gTwo :: Bool -> Bool -> Bool 2^2^2 = 2^(2*2) = 16
    -- fTwo :: Bool -> Quad -> Quad 4^(4*2) = 2^(2*4*2) = 65536

    data BinaryTree a =
        Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

    insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
    insert' b Leaf = Node Leaf b Leaf
    insert' b (Node left a right)
        | b == a = Node left a right
        | b < a = Node (insert' b left) a right
        | b > a = Node left a (insert' b right)

    -- filling in some details to help you along
    -- Note, you do *not* need to use insert' for this.
    -- Retain the original structure of the tree.
    mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    mapTree _ Leaf = Leaf
    mapTree f (Node left a right) =
        Node (mapTree f left) (f a) (mapTree f right)

    testTree' :: BinaryTree Integer
    testTree' =
        Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

    mapExpected =
        Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

    -- acceptance test for mapTree
    mapOkay =
        if mapTree (+1) testTree' == mapExpected
        then print "yup okay!"
        else error "test failed!"

    -- hints for implementing mapTree below this code block

    -- Chapter Exercises

    -- Multiple choice

    data Weekday = -- a) Weekday is a type with five data constructors
        Monday
        | Tuesday
        | Wednesday
        | Thursday
        | Friday

    f Friday = "Miller Time" -- f :: Weekday -> String

    -- Types defined with the data keyword - b) must begin with a capital letter

    g xs = xs !! (length xs - 1) -- c) delivers the final element of xs

    -- Vigenère cipher.

    cryptF x y = x + y

    decryptF x y = x - y

    getCO x = mod (26 + x) 26

    getFirstCharMode c = if (isUpper c) then (ord 'A') else (ord 'a')

    cryptCharF f o c = chr $ firstCharMode + (getCO (f charO o))
        where
        firstCharMode = getFirstCharMode c
        charO = ord c - firstCharMode

    ceaserCrypt o str = map crypt str
        where crypt = cryptCharF cryptF o

    ceaserDecrypt o str = map decrypt str
        where decrypt = cryptCharF decryptF o

    vigenereCrypt :: String -> String -> String
    vigenereCrypt cryptStr str = zipWith crypt offsetList str
        where crypt = cryptCharF cryptF
              normalize c = ord c - getFirstCharMode c
              offsetList = map normalize cryptStr

    -- As-patterns

    isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsequenceOf [] _ = True
    isSubsequenceOf _ [] = False
    isSubsequenceOf (s:sub) str = elem s str && Eleven.isSubsequenceOf sub str

    -- isSubSeqOf (x:xs) ys@(y:ys_tail)
    --     | x == y = isSubSeqOf xs ys
    --     | otherwise = isSubSeqOf (x:xs) ys_tail

    capitalize word@(first:rest) = (word, (toUpper first) : rest)

    capitalizeWords :: String -> [(String, String)]
    capitalizeWords str = map capitalize ws
        where ws = words str

    -- Language exercises

    capitalizeWord :: String -> String
    capitalizeWord (s:str) = toUpper s : str

    convo :: [String]
    convo =
        ["Wanna play 20 questions. Wanna",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

    -- validButtons = "1234567890*#"
    type Digit = Char
    -- Valid presses: 1 and up
    type Presses = Int

    data DaPhoneButton = DaPhoneButton Digit [Digit] [Digit] deriving (Eq, Show)

    data DaPhone = DaPhone [DaPhoneButton] deriving (Show)

    phone = DaPhone
        [   DaPhoneButton '1' ['1'] ['1']
        ,   DaPhoneButton '2' ['a', 'b', 'c', '2'] ['A', 'B', 'C', '2']
        ,   DaPhoneButton '3' ['d', 'e', 'f', '3'] ['D', 'E', 'F', '3']
        ,   DaPhoneButton '4' ['g', 'h', 'i', '4'] ['G', 'H', 'I', '4']
        ,   DaPhoneButton '5' ['j', 'k', 'l', '5'] ['J', 'K', 'L', '5']
        ,   DaPhoneButton '6' ['m', 'n', 'o', '6'] ['M', 'N', 'O', '6']
        ,   DaPhoneButton '7' ['p', 'q', 'r', 's', '7'] ['P', 'Q', 'R', 'S', '7']
        ,   DaPhoneButton '8' ['t', 'u', 'v', '8'] ['T', 'U', 'V', '8']
        ,   DaPhoneButton '9' ['w', 'x', 'y', 'z', '9'] ['W', 'X', 'Y', 'Z', '9']
        ,   DaPhoneButton '*' [] []
        ,   DaPhoneButton '0' ['+', ' ', '0'] []
        ,   DaPhoneButton '#' ['.', ','] []
        ]

    getPhoneButton :: DaPhone -> Char -> DaPhoneButton
    getPhoneButton (DaPhone buttons) c = head $ filter (\(DaPhoneButton _ lowercase uppercase) -> elem c lowercase || elem c uppercase) buttons

    getCapitalizePhoneNumber :: DaPhone -> DaPhoneButton
    getCapitalizePhoneNumber (DaPhone buttons) = head $ filter (\(DaPhoneButton _ lowercase uppercase) -> (length lowercase == 0) && (length uppercase == 0)) buttons

    isCharBig :: DaPhoneButton -> Char -> Bool
    isCharBig (DaPhoneButton _ _ uppercase) c = elem c uppercase

    reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
    reverseTaps phone c = if isBig then [(capitalizeDigit, 1), (digit, pressCount)] else [(digit, pressCount)]
        where
            button@(DaPhoneButton digit _ _) = getPhoneButton phone c
            pressCount = findIndx button c
            (DaPhoneButton capitalizeDigit _ _) = getCapitalizePhoneNumber phone
            isBig = isCharBig button c


    findIndx :: DaPhoneButton -> Char -> Int
    findIndx (DaPhoneButton _ lowercase uppercase) c = 1 + (length $ getIndx keys)
        where
            keys = zip lowercase uppercase
            getIndx = takeWhile (\(l, u) -> l /= c && u /= c)

    cellPhonesDead :: DaPhone
        -> String
        -> [(Digit, Presses)]
    cellPhonesDead phone str = concatMap (reverseTaps phone) str

    fingerTaps :: [(Digit, Presses)] -> Presses
    fingerTaps i = sum $ map (\(_, p) -> p) i

    getDigit :: (Digit, Presses) -> Digit
    getDigit (d, _) = d

    getPresses :: (Digit, Presses) -> Presses
    getPresses (_, p) = p

    -- getCharByButtonInfo

    -- mostPopularLetter :: String -> Char
    mostPopularLetter str = groupedLower
        where
            allKeys = map (reverseTaps phone) str
            lower = concat $ filter (\a -> length a == 1) allKeys
            sortedLower = sortBy (\b b' -> compare (getDigit b) (getDigit b')) lower
            groupedLower =
                (groupBy (\b b' -> (getDigit b) == (getDigit b')) sortedLower)
            countedLower =
                (map (\a ->
                    (getDigit $ head a
                    , foldr (\b acc -> (getPresses b) + acc) 0 a)
                    ) groupedLower
                )
            mostPopularLower = [head $ sortBy (\b b' -> compare (getPresses b') (getPresses b)) countedLower]
            upper = filter (\a -> length a > 1) allKeys
            sortedUpper = sortBy (\b b' -> compare (getDigit $ last b) (getDigit $ last b')) upper


            -- sorted = (sort (\b b' -> (getDigit $ head b) == (getDigit $ head b')) $ getLower lowerAndUpper, sort (\b b' -> getDigit b == getDigit b') $ getUpper lowerAndUpper)
            -- groupByLower = (\b b' -> getDigit b == getDigit b')
            -- groupByUpper = (\a a -> (getDigit tail d) == (getDigit tail d'))
            -- grouped

    -- ...

    -- Hutton’s Razor

    data Expr
        = Lit Integer
        | Add Expr Expr

    eval :: Expr -> Integer
    eval (Lit v) = v
    eval (Add exp exp') = (eval exp) + (eval exp')

    printExpr :: Expr -> String
    printExpr (Lit v) = show v
    printExpr (Add exp exp') = (printExpr exp) ++ " + " ++ (printExpr exp')