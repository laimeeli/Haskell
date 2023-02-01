{-# LANGUAGE FlexibleInstances, RebindableSyntax #-}

import Prelude hiding (Monad, (>>=), return)

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

-- Re-define meanings using do-notation

aRelativeDies :: [T]
aRelativeDies = do
 x <- aRelative
 return (dies x)

ifRelDiesHouse :: [T]
ifRelDiesHouse = do
  p <- aRelativeDies
  return (ifThen p house)

ifRelDiesHouseClosed :: T
ifRelDiesHouseClosed = do
  p <- aRelativeDies
  closure (return (ifThen p house))

-- Monad instance for list 
data List a = Empty | Add a (List a)
  deriving (Show, Eq)

instance Monad List where
  return x = Add x Empty
  m >>= f  = flatten (mapList f m)

mapList :: (a -> b) -> List a -> List b
mapList f Empty     = Empty
mapList f (Add h t) = Add (f h) (mapList f t)

concatList :: List a -> List a -> List a
concatList Empty xs     = xs
concatList (Add h t) xs = Add h (concatList t xs)

flatten :: List (List a) -> List a
flatten Empty     = Empty
flatten (Add h t) = concatList h (flatten t)

-- Define functor and applicative instance

instance Functor List where
  fmap f Empty     = Empty
  fmap f (Add h t) = Add (f h) (fmap f t)

instance Applicative List where
  pure x             = Add x Empty
  Empty      <*> xs  = Empty
  (Add f fs) <*> xs  = concatList (fmap f xs) (fs <*> xs)

-- do-notation style meaning derivation

aRelativeList :: List E
aRelativeList = Add alf (Add bea Empty)

aRelativeDiesList :: List T
aRelativeDiesList = do
  x <- aRelativeList
  return (dies x)

ifRelDiesHouseList :: List T
ifRelDiesHouseList = do
  p <- aRelativeDiesList
  return (ifThen p house)
 
-- Define variables 

data Var = V | X | Y | Z

type Assignment = Var -> E

-- Define type-constructor that makes things assignment sensitive

type G a = Assignment -> a

she_V, she_X, she_Y, she_Z :: G E -- you can declare multiple types at once!
she_V = \g -> g V
she_X = \g -> g X
she_Y = \g -> g Y
she_Z = \g -> g Z

-- Define a monad instance based on G

instance Monad ((->) Assignment) where -- (->) Assignment is another way of
                                       -- writing our type constructor G!
  return x = \g -> x
  m >>= f  = \g -> f (m g) g

-- do-notation style derivation of assignment-sensitive meanings 

sheXDies :: G T
sheXDies = do
  x <- she_X
  return (dies x)

ifSheXDiesHouse :: G T
ifSheXDiesHouse = do
  p <- sheXDies
  return (ifThen p house)

-- make Ga displayable 

gStart :: Assignment
gStart V = alf
gStart X = bea
gStart Y = cat
gStart Z = dan

eval :: G a -> a
eval m = m gStart

-- Auxiliary definitions 

type E = String

alf :: E
alf = "Alf"

bea :: E
bea = "Bea"

cat :: E
cat = "Cat"

dan :: E
dan = "Dan"

aRelative :: [E]
aRelative = [alf, bea]

aLawyer :: [E]
aLawyer = [cat, dan]

type T = String

dies :: E -> T
dies x = x ++" dies"

visits :: E -> E -> T
visits x y = y ++" visits "++ x

ifThen :: T -> T -> T
ifThen p q = "if "++ p ++" then "++ q

house :: T
house = "I'll inherit a house!"

closure :: [T] -> T
closure []     = ""
closure (p:[]) = p
closure (p:ps) = p ++" or "++ closure ps