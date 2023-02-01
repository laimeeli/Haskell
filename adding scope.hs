{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- Higher order meanings 

aLawyerVisitsARelative1 :: [[T]]
aLawyerVisitsARelative1 = do 
  x <- aLawyer
  return  (do 
     y <- aRelative
     return (visits y x))


aLawyerVisitsARelative2 :: [[T]]
aLawyerVisitsARelative2 = do
  y <- aRelative
  return  (do 
     x <- aLawyer
     return (visits y x))

-- Derive Exceptional scope readiings

sel1 :: [T]
sel1 = do
  m <- aLawyerVisitsARelative1
  return (ifThen (closure m) house)

sel2 :: [T]
sel2 = do
  m <- aLawyerVisitsARelative2
  return (ifThen (closure m) house)

-- Adding assignment sensitivity
> -- (data) types for variables, assignments, and assignment-dependent things
> data Var = V | X | Y | Z deriving Eq
> type Assignment = Var -> E
> type G a = Assignment -> a

  -- entries for pronouns
she_V, she_X, she_Y, she_Z :: G E
she_V = \g -> g V
she_X = \g -> g X
she_Y = \g -> g Y
she_Z = \g -> g Z

  -- a starting assignment function
gStart :: Assignment
gStart V = alf
gStart X = bea
gStart Y = cat
gStart Z = dan

  -- a function that evaluates an assignment-dependent thing at gStart
eval :: G a -> a
eval m = m gStart

-- sample assignment-sensitive meaning derived monadically 

sheXVisitsAlf :: G T
sheXVisitsAlf = do x <- she_X
           return (visits alf x)
           
-- Getting the ambiguity of a sentence (two derivations) 

sheYVisitsARelative1 :: G [T]
sheYVisitsARelative1 = do
  x <- she_Y
  return (do
    y <- aRelative
    return (visits y x))

sheYVisitsARelative2 :: [G T]
sheYVisitsARelative2 = do
  y <- aRelative
  return (do 
    x <- she_Y
    return (visits y x))

-- Adding binding 

modify :: Var -> E -> Assignment -> Assignment
modify var x g = \var' -> if var == var' then x else g var'

-- Derive the bound reading of "Bea visits herself" 

beta :: Var -> (E -> G a) -> E -> G a
beta var f x = \g -> f x (modify var x g)

beaVisitsHerself :: G T
beaVisitsHerself = beta V (\x -> (do
  y <- she_V
  return (visits y x ))) bea
  
-- Derive the bound reading of "a relative visits herself" 

aRelativeVisitsHerself :: [G T]
aRelativeVisitsHerself = do 
  x <- aRelative
  return  (beta V (\x -> (do 
    y <- she_V
    return (visits y x))) x )