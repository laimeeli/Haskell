-- Define strings 

aString :: String
aString = "WAT"

aSentenceyString :: String
aSentenceyString = "John walked in the park."

-- Define entities  
type E = String -- this just defines a type synonym
                -- things of type E are just String's
                -- which are just sequences of characters

alf :: E
alf = "Alf"

bea :: E
bea = "Bea"

cat :: E
cat = "Cat"

dan :: E
dan = "Dan"

-- Define verby things 

type T = String

dies :: E -> T
dies x = x ++" dies"

visits :: E -> E -> T
visits x y = y ++" visits "++ x

-- Some operators 

and' :: T -> T -> T 
and' q p = p ++ "and" ++ q

didn't :: E -> T -> T
didn't p x = x ++ "didn't" ++ p

-- Define indefinites 

aRelative :: [E]
aRelative = [alf, bea]

aLawyer :: [E]
aLawyer = [cat, dan]

-- Now we can use "map" to define meanings for "a lawyer dies" and "visits a relative" 

aLawyerDies :: [T]
aLawyerDies =  map dies aLawyer

visitsARelative :: [E -> T]
visitsARelative =  map visits aRelative

-- Alternative derivation using another version of 'map' 

flipMap :: [a] -> (a -> b) -> [b]
flipMap xs f = map f xs
aLawyerDiesNew :: [T]
aLawyerDiesNew = flipMap aLawyer (\x -> dies x)
