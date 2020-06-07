nico :: Persona
maiu :: Persona

data Persona = UnaPersona String Float Int [(String,Int)] deriving(Show)
-- Nombre, Dinero, Suerte, Factores (Inteligencia, amuletos, etc.)

nico = (UnaPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (UnaPersona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])

-- 1.
amuleto :: String
amuleto = "amuleto"

factores :: Persona -> [(String,Int)]
factores (UnaPersona _ _ _ listaFactores) = listaFactores

dinero :: Persona -> Float
dinero (UnaPersona _ d _ _) = d

suerte :: Persona -> Int
suerte (UnaPersona _ _ valorSuerte _) = valorSuerte

tieneFactor :: String -> Persona -> Bool
tieneFactor factor = elem factor.map fst.factores

esAmuleto :: (String,Int) -> Bool
esAmuleto = (==amuleto).fst

multiplicadorAmuleto :: [(String,Int)] -> Int
multiplicadorAmuleto = snd.head.filter esAmuleto

suerteTotal :: Persona -> Int
suerteTotal persona
  | tieneFactor amuleto persona && valorAmuleto > 0 = (*) valorAmuleto.suerte $ persona
  | otherwise = suerte persona
    where
        valorAmuleto = multiplicadorAmuleto.factores $ persona

-- 2.
data Juego = UnJuego {
    nombre :: String,
    funcionDinero :: (Float -> Float),
    criterios :: [(Persona -> Bool)]
}

ruleta :: Juego
ruleta = UnJuego {
    nombre = "Ruleta",
    funcionDinero = (*37),
    criterios = [(>80).suerteTotal]
}

maquinita :: Float -> Juego
maquinita jackpot = UnJuego {
    nombre = "Maquinita",
    funcionDinero = (+) jackpot,
    criterios = [(>95).suerteTotal, tieneFactor "paciencia"]
}

-- 3.
puedeGanar :: Persona -> Juego -> Bool
puedeGanar persona = all (==True).map ($ persona).criterios

-- 4.a
casino777 :: [Juego]
casino777 = [ruleta, maquinita 20000]

calcularTotalGanancias :: Persona  -> [Juego] -> (Float -> Float)
calcularTotalGanancias persona juegos
  | (>0).length.funcionesGanancia $ juegos = foldl1 (flip (.)).funcionesGanancia $ juegos
  | otherwise = (*0)
    where
        funcionesGanancia = map funcionDinero.filter (puedeGanar persona)

totalGanancias :: Persona -> Float -> [Juego] -> Float
totalGanancias persona apuestaInicial juegos = calcularTotalGanancias persona juegos apuestaInicial

--4.b
totalGananciasRecursividad :: Persona -> Float -> [Juego] -> Float
totalGananciasRecursividad _ _ [] = 0
totalGananciasRecursividad persona apuestaInicial (x:xs)
  | puedeGanar persona x = (+) (totalGananciasRecursividad persona apuestaInicial xs).funcionDinero x $ apuestaInicial
  | otherwise = totalGananciasRecursividad persona apuestaInicial xs

  {-
    *Main> totalGanancias nico (dinero nico) casino777
    3700.0

    *Main> totalGananciasRecursividad nico (dinero nico) casino777
    3700.0
    
    *Main> totalGananciasRecursividad maiu (dinero maiu) casino777
    0.0

    *Main> totalGanancias maiu (dinero maiu) casino777
    0.0
  -}