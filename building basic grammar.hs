data List a = Empty | Add a (List a)
  deriving (Show, Eq)

-- Hand-rolled lists 
headList :: List a -> a
headList (Add a Empty) = a
headList (Add a b) = headList b

tailList :: List a -> List a
tailList (Add a Empty) = Empty
tailList (Add a b) = Add a (tailList b)

mapList :: (a -> b) -> List a -> List b
mapList f Empty = Empty
mapList f (Add h t) = Add (f(h))(mapList f(t))

concatList :: List a -> List a -> List a
concatList Empty xs = xs
concatList (Add h t) xs = Add h (concatList t xs)

flatten :: List (List a) -> List a
flatten Empty = Empty
flatten (Add h t) = concatList h (flatten t)

-- Define functions using lists 
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f(x) : myMap f(xs)

myConcat :: [a] -> [a] -> [a]
myConcat [] xs = xs
myConcat (x:y) xs = x : (myConcat y xs)

myFlat :: [[a]] -> [a]
myFlat [] = []
myFlat (x:xs) = myConcat x (myFlat xs)

-- Define mapping using list comprehensions
mapComp ::  (a -> b) -> [a] -> [b]
mapComp f [] = []
mapComp f (x:xs) = [f (a)|a <- (x:xs)]

-- Implementing type shifters 
  -- Partee's A-shiter 
  reList :: a -> [a]
  reList x = [x]
    --It reports a -> (a -> Bool) -> Bool, which should be the right type of LIFT
bindList m f = myFlat (mapComp f (m))
    -- It reports back the type of bindList. reList as a1 -> (a1 -> [a]) -> [a], which looks like the type of operation on the right hand side of LIFT. 