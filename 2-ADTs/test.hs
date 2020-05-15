data Thing = Shoe
            | Ship
            | SealingWax
            | Cabbage
            | King
        deriving Show

-- A function isSmall on the enumeration Thing
isSmall :: Thing -> Bool
isSmall King = False
isSmall Ship = False
isSmall _    = True

-- Beyond Enumeration
data FailableDoube = Failure
                    | OK Double
                deriving Show

safeDiv :: Double -> Double -> FailableDoube
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- Store a Person name, age and favourite Thing
data Person = Person String Int Thing
    deriving Show
    
naveen :: Person
naveen = Person "Naveen" 21 Cabbage

sam :: Person
sam = Person "Sam" 18 Shoe

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name of the field of (" ++ show p ++ ") is " ++ n

-- Another way of defining Person
data Person1 = Person1 { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)  