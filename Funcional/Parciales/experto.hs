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

nombrePersona :: Persona -> String
nombrePersona (UnaPersona n _ _ _) = n

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

-- 5.
pierdeTodo :: Persona -> [Juego] -> Bool
pierdeTodo persona = all (==False).map (puedeGanar persona)

perdedores :: [Persona] -> [Juego] -> [String]
perdedores personas juegos = map nombrePersona.filter (`pierdeTodo` juegos) $ personas

-- 6.
cambiarDinero :: Persona -> Float -> Persona
cambiarDinero persona monto = (UnaPersona (nombrePersona persona) ((+monto).dinero $ persona) (suerte persona) (factores persona))

apostar :: Persona -> Float -> Juego -> Persona
apostar persona apuesta juego
  | (<apuesta).dinero $ persona = persona
  | puedeGanar persona juego = cambiarDinero persona ganancia
  | otherwise = cambiarDinero persona (-apuesta)
      where
        ganancia = (funcionDinero juego) $ apuesta

-- 7.
{- 

elCocoEstaEnLaCasa :: (Num a, Ord a, Foldable t) => (_, a) -> (c -> [a]) -> a -> t [(a -> a,c)] -> Bool
elCocoEstaEnLaCasa x y z = all ((>z).(+42)).foldl (\a (b,c) -> y c ++ b a) (snd x)

-- foldl funcion semilla lista => el último parametro es una lista. A este parametro point-free lo denominaremos _j_.
-- (\a (b,c) -> y c ++ b a) => funcion lambda que recibe a y una tupla. _b_ e _y_ son funciones que devuelven una lista.
-- (snd x) => _x_ es una tupla. Su segundo parametro es _a_.
-- de la funcion lambda, a es una lista.
-- _j_ es una lista de tuplas cuyos primeros componentes son _b_ y sus segundos componentes son _c_.
-- _z_ es un valor numerico ya que (>z).(+42) recibe un valor numérico al que se le suma 42 y posteriormente debe poder compararse con _z_. _z_ es _a_.
-- _b_ recibe _a_ y devuelve _a_.
-- _j_ tiene que ser foldeable.
-- elCocoEstaEnLaCasa devuelve un Bool (la última función es all)

-}