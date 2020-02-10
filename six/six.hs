module Six where
  import Data.List

  data TisAnInteger =
    TisAn Integer

  instance Eq TisAnInteger where
    (==) (TisAn number)
         (TisAn number') = number == number'

  data TwoIntigers =
    Two Integer Integer

  instance Eq TwoIntigers where
    (==) (Two numberLeft numberLeft')
         (Two numberRigth numberRigth') =
          numberLeft == numberRigth && numberLeft' == numberRigth'

  data StringOrInt =
    TisAnInt Int
    | TisAString String

  instance Eq StringOrInt where
    (==) (TisAnInt num)
         (TisAnInt num') = num == num'
    (==) (TisAString str)
         (TisAString str') = str == str'
    (==) _ _ = False

  data Pair a =
    Pair a a

  instance Eq a => Eq (Pair a) where
    (==) (Pair l l')
         (Pair r r') =
          l == r && l' == r'

  data Tuple a b =
    Tuple a b

  instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple v g)
         (Tuple v' g') =
          v == v' && g == g'

  data Which a =
    ThisOne a
    | ThatOne a

  instance Eq a => Eq (Which a) where
    (==) (ThisOne v)
         (ThisOne v') = v == v'
    (==) (ThatOne v)
         (ThatOne v') = v == v'
    (==) (ThisOne v)
         (ThatOne v') = v == v'
    (==) _ _ = False

  data EitherOr a b =
    Hello a
    | Goodbye b

  instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello v)
         (Hello v') = v == v'
    (==) (Goodbye v)
         (Goodbye v') = v == v'
    (==) _ _ = False

  data Person = Person Bool

  instance Show Person where
    show (Person bool) = "Person" ++ " " ++ (show bool)

  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  data Mood = Blah
    | Woot deriving (Show, Eq)

  settleDown x = if x == Woot
                 then Blah
                 else x

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

  data Rocks = Rocks String deriving (Eq, Show)

  data Yeah = Yeah Bool deriving (Eq, Show)

  data Papu = Papu Rocks Yeah deriving (Eq, Show)

  freud :: Int -> Int
  freud x = x

  jung :: [Int] -> Int
  jung xs = head (sort xs)

  mySort :: [Char] -> [Char]
  mySort = sort

  signifier xs = head (mySort xs)

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk aToB a b = (aToB a) == b

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith aToB num a = (aToB a) + (fromIntegral num)