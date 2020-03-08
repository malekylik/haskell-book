module Twelve where

    import Data.Char
    import Data.List

    -- Determine the kinds
    -- id :: a -> a -- *
    -- r :: a -> f a -- * -> *

    -- String processing

    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe s     = Just s

    replaceThe :: String -> String
    replaceThe s = substitiuted ws
        where
            ws = words s
            substite Nothing = "a"
            substite (Just s) = s
            substitiuted [] = []
            substitiuted (x:xs) = (substite $ notThe x) ++ " " ++ (substitiuted xs)

    isVowel c = elem c "aeiou"
    isStartFromVowels w = isVowel (head w)

    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel sentence = go ws 0
        where
            ws = words sentence
            isThe Nothing = True
            isThe (Just s) = False
            go [] count = count
            go [_] count = count
            go (w1:w2:ws) count = if (isThe . notThe $ map toLower w1) && isStartFromVowels w2 then 1 + go (w2:ws) count else 0 + go (w2:ws) count

    countVowels :: String -> Integer
    countVowels s = toInteger . length $ vovels
        where
            vovels = filter isVowel s

    newtype Word' =
        Word' String
        deriving (Eq, Show)

    isConsonant a = (not . isVowel) a

    mkWord :: String -> Maybe Word'
    mkWord word = if vowelCount > consonantCount then Just (Word' word) else Nothing
        where 
            vowelCount = countVowels word
            consonantCount = (toInteger (length word)) - vowelCount

    -- As natural as any competitive bodybuilder
    data Nat =
        Zero
        | Succ Nat
        deriving (Eq, Show)

    -- >>> natToInteger Zero
    -- 0
    -- >>> natToInteger (Succ Zero)
    -- 1
    -- >>> natToInteger (Succ (Succ Zero))
    -- 2
    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ v) = 1 + (natToInteger v)

    integerToNat :: Integer -> Maybe Nat
    integerToNat i
        | i < 0 = Nothing
    integerToNat i = Just $ converter i
        where
            converter 0  = Zero
            converter i  = Succ $ converter $ i - 1

    -- >>> isJust (Just 1)
    -- True
    -- >>> isJust Nothing
    -- False
    isJust :: Maybe a -> Bool
    isJust v = case v of Nothing -> False
                         (Just _) -> True

    -- >>> isNothing (Just 1)
    -- False
    -- >>> isNothing Nothing
    -- True
    isNothing :: Maybe a -> Bool
    isNothing v = case v of Nothing -> True
                            (Just _) -> False

    -- >>> mayybee 0 (+1) Nothing
    -- 0
    -- >>> mayybee 0 (+1) (Just 1)
    -- 2
    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee init _ Nothing = init
    mayybee init cb (Just v) = cb v

    -- Try writing it in terms of the maybe catamorphism

    -- >>> fromMaybe 0 Nothing
    -- 0
    -- >>> fromMaybe 0 (Just 1)
    -- 1
    fromMaybe :: a -> Maybe a -> a
    fromMaybe i m = mayybee i id m

    -- >>> listToMaybe [1, 2, 3]
    -- Just 1
    -- >>> listToMaybe []
    -- Nothing
    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

    -- >>> maybeToList (Just 1)
    -- [1]
    -- >>> maybeToList Nothing
    -- []
    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just v) = [v]

    -- >>> catMaybes [Just 1, Nothing, Just 2]
    -- [1, 2]
    -- >>> catMaybes [Nothing, Nothing, Nothing]
    -- []
    catMaybes :: [Maybe a] -> [a]
    catMaybes l = map (\(Just v) -> v) filtered
        where
            filterF Nothing = False
            filterF (Just _) = True
            filtered = filter filterF l

    -- >>> flipMaybe [Just 1, Just 2, Just 3]
    -- Just [1, 2, 3]
    -- >>> flipMaybe [Just 1, Nothing, Just 3]
    -- Nothing
    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe l = if isFalsy then Nothing else Just $ map (\(Just v) -> v) l
        where
            isFalsy = length (filter filterF l) /= 0
            filterF Nothing = True
            filterF _ = False

    -- Small library for Either
    lefts' :: [Either a b] -> [a]
    lefts' xs = map (\(Left v) -> v) filtered
        where
            filtered = filter filteredFunc xs
            filteredFunc x = case x of (Right _) -> False
                                       (Left _)  -> True

    rights' :: [Either a b] -> [b]
    rights' xs = map (\(Right v) -> v) filtered
        where
            filtered = filter filteredFunc xs
            filteredFunc x = case x of (Right _) -> True
                                       (Left _)  -> False

    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' xs = (map (\(Left x) -> x) lefts, map (\(Right x) -> x) rights)
        where
            rights = filter filteredLeft xs
            lefts  = filter filteredRight xs
            filteredRight x = case x of (Right _) -> False
                                        (Left _)  -> True
            filteredLeft x  = case x of (Right _) -> True
                                        (Left _)  -> False

    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' _ (Left v) = Nothing
    eitherMaybe' f (Right v) = Just $ f v

    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either' f _ (Left a)  = f a
    either' _ f (Right a) = f a

    eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe'' f x = either' (\_ -> Nothing) (\x -> Just $ f x) x
