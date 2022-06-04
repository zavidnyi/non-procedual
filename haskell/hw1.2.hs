import Distribution.Simple.Setup (trueArg)
-- import Distribution.Compat.Lens (_1)
data Velikost = Mala | Velka
    deriving (Show)

data Omacka = Rajcatova | Smetanova | Pesto
    deriving (Show)

data Prisada = Salam | Sunka | Cibule | Ananas | Kure
    deriving (Show)

data Dresink = Caesar | Olej | Dijonsky | Jogurtovy | Zadny
    deriving (Show)

data Jidlo = Pizza Velikost Omacka [Prisada]
           | Salat Dresink
           | Bageta Velikost Dresink
           deriving (Show)


cenaPrisady :: Velikost -> Prisada -> Int
cenaPrisady velikost prisada =
    case (velikost, prisada) of
        (Mala, Kure) -> 60
        (Velka, Kure) -> 75
        (Mala, _) -> 30
        (Velka, _) -> 50
 
cenaOmacky :: Velikost -> Omacka -> Int
cenaOmacky velikost omacka =
    case (velikost, omacka) of
        (Mala, Pesto) -> 30
        (Velka, Pesto) -> 40
        (_, _) -> 0

hladoveji :: Jidlo -> Jidlo
hladoveji (Pizza _ a b) = Pizza Velka a b
hladoveji (Bageta _ a) = Bageta Velka a
hladoveji a = a 

pizzaToppingThief :: [Prisada] -> [Prisada]
pizzaToppingThief = map replaceJunk 
    where replaceJunk top = case top of Sunka -> Kure 
                                        Salam -> Kure 
                                        _ -> top


zdraveji :: Jidlo -> Jidlo
zdraveji (Salat _) = Salat Zadny
zdraveji (Bageta a _) = Bageta a Zadny
zdraveji (Pizza a b toppings) = Pizza a b (pizzaToppingThief toppings)

-- cenaJidla :: Jidlo -> Int
-- cenaJidla (Bageta Velka _) = 120
-- cenaJidla (Bageta Mala _) = 90
-- cenaJidla (Pizza velikost omacka )

-- nejvicePrisad :: [Jidlo] -> Int
-- nejvicePrisad

sunkovitost :: [Jidlo] -> Int
sunkovitost [] = 0
sunkovitost ((Pizza v o prisady): rest) = 
    let
        sunko = if any isSunka prisady then 1 else 0
    in sunko + sunkovitost rest
    where isSunka prisada = case prisada of Sunka -> True 
                                            _ -> False
sunkovitost (_:rest) = sunkovitost rest