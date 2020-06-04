module Lib where

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: (Ord a) => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------

-- Punto 1
type Efecto = Jugador -> Int
type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

putter :: Palo
putter skill = UnTiro {
  velocidad = 10,
  precision = (*2).precisionJugador $ skill,
  altura = 0
}

madera :: Palo
madera skill = UnTiro {
  velocidad = 100,
  precision = (`div` 2).precisionJugador $ skill,
  altura = 5
}

hierro :: Int -> Palo
hierro n skill = UnTiro {
  velocidad = (*n).fuerzaJugador $ skill,
  precision = (`div` n).precisionJugador $ skill,
  altura = (min 0).(+ (-3))  $ n
}

-- Punto 2
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo.habilidad $ jugador

-- Punto 3

type Condicion = Tiro -> Bool
type EfectoTiro = Tiro -> Tiro

modificarVelocidad :: (Int -> Int) -> Tiro -> Tiro
modificarVelocidad f tiro = tiro { velocidad = f.velocidad $ tiro }

modificarAltura :: (Int -> Int) -> Tiro -> Tiro
modificarAltura f tiro = tiro { altura = f.altura $ tiro }

modificarPrecision :: (Int -> Int) -> Tiro -> Tiro
modificarPrecision f tiro = tiro { precision = f.precision $ tiro }

data Obstaculo = UnObstaculo{
  condiciones :: [Condicion],
  efectos :: EfectoTiro
}

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo {
  condiciones = [(>90).precision, (==0).altura],
  efectos = modificarVelocidad (*2).modificarPrecision ((+100).(*0)).modificarAltura (*0)
}

laguna :: Int -> Obstaculo
laguna largo = UnObstaculo {
  condiciones = [(>80).velocidad, between 1 5.altura],
  efectos = modificarVelocidad (*1).modificarPrecision (*1).modificarAltura (`div` largo)
}

hoyo :: Obstaculo
hoyo = UnObstaculo {
  condiciones = [between 5 20.velocidad,(==0).altura,(>95).precision],
  efectos = modificarVelocidad (*0).modificarPrecision (*0).modificarAltura (*0)
}

superaCondicion :: Condicion -> Tiro -> Bool
superaCondicion condicion tiro = condicion tiro

superaCondiciones :: Obstaculo -> Tiro -> Bool
superaCondiciones obstaculo tiro = all (==True).map ($ tiro).condiciones $ obstaculo

tiroTrasObstaculo :: Obstaculo -> Tiro -> Tiro
tiroTrasObstaculo obstaculo tiro
  | superaCondiciones obstaculo tiro = (efectos obstaculo) tiro
  | otherwise = (UnTiro 0 0 0)

-- Punto 4
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaCondiciones obstaculo.golpe jugador) palos

obstaculos :: [Obstaculo]
obstaculos = [tunelConRampita, tunelConRampita, hoyo]

tiroPrueba :: Tiro
tiroPrueba = UnTiro {
  velocidad = 10,
  precision = 95,
  altura = 0
}

superarConsecutivamente :: [Obstaculo] -> Tiro -> Int
superarConsecutivamente [] _ =  0
superarConsecutivamente (x:xs) tiro
  | superaCondiciones x tiro = (+) 1 (superarConsecutivamente xs tiro)
  | otherwise = (+) 0 (superarConsecutivamente xs tiro)

superarConsecutivamenteBonus :: [Obstaculo] -> Tiro -> Int
superarConsecutivamenteBonus listaObstaculos tiro = length.takeWhile (`superaCondiciones` tiro) $ listaObstaculos

-- Punto 5
type Competidor = (Jugador, Puntos)
type Puntajes = [Competidor]

listaPuntajes :: Puntajes
listaPuntajes = [(bart, 90),(todd, 90),(rafa, 5)]

mayorPuntaje :: Puntajes -> Puntos
mayorPuntaje puntajes = maximum.map snd $ puntajes

filtroPerdedor :: Puntajes -> Competidor -> Bool
filtroPerdedor puntajes = (< (mayorPuntaje puntajes)).snd

primerLugar :: Puntajes -> Puntajes
primerLugar puntajes = filter (not.filtroPerdedor puntajes) puntajes

padresDePerdedores :: Puntajes -> [String]
padresDePerdedores puntajes
  | (length.primerLugar $ puntajes) == 1 = map (padre.fst).filter (/= (head.primerLugar $ puntajes)) $ puntajes
  | otherwise = map (padre.fst) puntajes