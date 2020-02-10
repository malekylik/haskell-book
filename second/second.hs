module Learn where

  awesome = ["Papuchon", "curry", ":)"]
  alsoAwesome = ["Quake", "The Simons"]
  allAwesome = [awesome, alsoAwesome]

  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome x = x == reverse x

  myAbs :: Integer -> Integer
  myAbs x = if x < 0 then -x else x

  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f x y = ((snd x, snd y), (fst x, fst y))

  x = (+)
  ff xs = w `x` 1
    where w = length xs
