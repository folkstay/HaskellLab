type Name = String
type Hunger = Int
type Mood = Int
type Lives = Int
type Pickyness = Int
data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger deriving (Show, Eq)

data CatData = CatData
  { name :: Name
  , hunger :: Hunger
  , mood :: Mood
  , lives :: Lives
  , pickyness :: Pickyness
  , breed :: Breed
  } deriving (Show)

type Cat = forall r. (CatData -> r) -> r 

cat :: CatData -> Cat
cat catData = \msg -> msg catData

feedCat :: Cat -> Hunger -> Cat
feedCat sCat amountOfFood = sCat (\cd ->
  if hunger cd + amountOfFood > 100
  then cat $ cd { hunger = 100 }
  else cat $ cd { hunger = hunger cd + amountOfFood })

strokeCat :: Cat -> Mood -> Cat
strokeCat sCat strokeForce = sCat (\cd ->
  if mood cd + strokeForce > 100
  then cat $ cd { mood = 100 }
  else cat $ cd { mood = mood cd + strokeForce })

printCatInfo :: Cat -> String
printCatInfo sCat = sCat (\cd ->
  "Name: " ++ name cd ++ 
  ", Hunger: " ++ show (hunger cd) ++ 
  ", Mood: " ++ show (mood cd) ++ 
  ", Live: " ++ show (lives cd) ++ 
  ", Pickyness: " ++ show (pickyness cd) ++ 
  ", Breed: " ++ show (breed cd))

isHere :: Cat -> Bool
isHere sCat = sCat (\cd -> hunger cd > 0 && mood cd > 0)

spendTime :: Cat -> Cat
spendTime sCat = sCat (\cd ->
  cat $ cd { hunger = max 0 (hunger cd - pickyness cd)
           , mood = max 0 (mood cd - pickyness cd)
           , lives = lives cd + 1 })

battleRound (catA, catB) = (newA, newB)
  where
    newA = catA (\cdA ->
             catB (\cdB ->
               let (newH, newM) = if hunger cdB > 0 && mood cdB > 0
                                  then (max 0 (hunger cdA - pickyness cdA), max 0 (mood cdA - pickyness cdA))
                                  else (hunger cdA, mood cdA)
               in cat $ cdA { hunger = newH, mood = newM, lives = lives cdA + 1 }))
    newB = catB (\cdB ->
             catA (\cdA ->
               let (newH, newM) = if hunger cdA > 0 && mood cdA > 0
                                  then (max 0 (hunger cdB - pickyness cdB), max 0 (mood cdB - pickyness cdB))
                                  else (hunger cdB, mood cdB)
               in cat $ cdB { hunger = newH, mood = newM, lives = lives cdB + 1 }))

getWinner :: Cat -> Cat -> Cat
getWinner catA catB =
  let livesA = catA lives
      livesB = catB lives
  in if livesA > livesB
     then catA
     else catB