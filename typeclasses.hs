import Data.List

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek = Mon
                | Tue
                | Wed
                | Thu
                | Fri
                | Sat
                | Sun
    deriving  Show

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
             weekday == weekday' && dayOfMonth == dayOfMonth'

instance Show Date where
    show (Date weekday dayOfMonth) = show weekday ++ " " ++ show dayOfMonth

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int 
                 | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a a') (Pair b b') = a == b && a' == b'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a a') (Tuple b b') = a == b && a' == b'

data Which a =
    ThisOne a
    | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b = Hello a 
                  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                then
                    Blah
                else
                    x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Sentence
s1 = Sentence "tests" "test" "test"

s2 :: Object -> Sentence
s2 = Sentence "blah" "blah"

data Rocks =
    Rocks String deriving (Eq, Show, Ord)

data Yeah =
    Yeah Bool deriving (Eq, Show, Ord)

data Papu =
    Papu Rocks Yeah deriving (Eq, Show, Ord)

equalityFOrall :: Papu -> Papu -> Bool
equalityFOrall p p'  = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

f :: RealFrac a => a
f = 1.0

freud' :: Int -> Int
freud' x= x

jung :: [Int] -> Int
jung xs = head (sort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk ab a b = (ab a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith ab i a = (ab a)   + (fromInteger i)