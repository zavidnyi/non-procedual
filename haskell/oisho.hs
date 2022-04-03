data Planeta = Merkur | Venuse | Zeme | Mars | Jupiter | Saturn | Uran | Neptune
    deriving Show

pozemskyRok :: Float
pozemskyRok = 31557600.0

merkurRok :: Float 
merkurRok = pozemskyRok * 0.2408467

venuseRok :: Float
venuseRok = pozemskyRok * 0.61519726

marsRok :: Float
marsRok = pozemskyRok * 1.8808158

jupiterRok :: Float
jupiterRok = pozemskyRok * 11.862615

saturnRok :: Float
saturnRok = pozemskyRok * 29.447498

uranRok :: Float 
uranRok = pozemskyRok * 84.016846

neptuneRok :: Float
neptuneRok = pozemskyRok * 164.79132


vesmirnyVek :: Planeta -> Float -> Float
vesmirnyVek planeta vek = 
    case planeta of
        Merkur -> vek / merkurRok
        Venuse -> vek / venuseRok
        Zeme -> vek / pozemskyRok
        Mars -> vek / marsRok
        Jupiter -> vek / jupiterRok
        Saturn -> vek / saturnRok
        Uran -> vek / uranRok
        Neptune -> vek / neptuneRok

