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
  | tieneFactor amuleto persona = (*) (multiplicadorAmuleto.factores $ persona).suerte $ persona
  | otherwise = suerte persona

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

 maquinita :: Int -> Juego
 maquinita jackpot = UnJuego {
    nombre = "Maquinita",
    funcionDinero = (+jackpot),
    criterios = [(>95).suerteTotal, tieneFactor "paciencia"]
}

-- 3.