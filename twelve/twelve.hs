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


