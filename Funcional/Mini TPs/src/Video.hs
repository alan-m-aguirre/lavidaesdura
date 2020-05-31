module Video where

type Duracion = (Int,Int,Int)

-- Definir acá el data Video usando la notación que vimos en clase
data Video = Video {
    titulo :: String,
    duracion :: Duracion,
    hashtags :: String
} deriving (Show, Eq)

-- Crear los siguientes videos de prueba con las duraciones
-- que se indicaban en la consigna:
videoMuyLargo :: Video
videoMuyLargo = Video {
    titulo = "The Bride of Frankenstein",
    hashtags = "#Película #Universal #1935 #BorisKarloff",
    duracion = (1,15,45)
}

videoNormal :: Video
videoNormal = Video {
    titulo = "Primer Clase PDEP",
    hashtags = "#PdeP #Educativo #Miercoles #ClaseOnline",
    duracion = (0,32,12)
}

videoCorto :: Video
videoCorto = Video {
    titulo = "Tutorial Git - Branches y PRs",
    hashtags = "#PdeP #Educativo #Git #Miercoles #Tutorial",
    duracion = (0,10,59)
}

otroVideoCorto :: Video
otroVideoCorto = Video {
    titulo = "Cygnet Committee",
    hashtags = "#Música #DavidBowie #ProgressiveRock #SpaceOddity",
    duracion = (0,10,20)
}