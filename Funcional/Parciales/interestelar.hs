posicion :: Planeta -> Posicion
tiempo :: Planeta -> (Float -> Float)
coordX :: Posicion -> Float
coordY :: Posicion -> Float
coordZ :: Posicion -> Float
nombre :: Astronauta -> String
edad :: Astronauta -> Float
planeta :: Astronauta -> Planeta

data Planeta = UnPlaneta String Posicion (Float -> Float)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

data Astronauta = UnAstronauta String Float Planeta
nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p

planeta1 :: Planeta
planeta1 = UnPlaneta "ABC" (1,2,3) (*2)

planeta2 :: Planeta
planeta2 = UnPlaneta "ABC" (7,6,5) (*2)

niel :: Astronauta
niel = (UnAstronauta "Niel" 40 planeta1)

-- 1.a
diferenciaCoord :: (Posicion -> Float) -> Planeta -> Planeta -> Float
diferenciaCoord coord planetaA = (-) (coord.posicion $ planetaA).coord.posicion

distanciaPlanetas :: Planeta -> Planeta -> Float
distanciaPlanetas planetaA planetaB =  sqrt (diffX + diffY + diffZ)
  where 
      diffX = (**2).diferenciaCoord coordX planetaA $ planetaB
      diffY = (**2).diferenciaCoord coordY planetaA $ planetaB
      diffZ = (**2).diferenciaCoord coordZ planetaA $ planetaB

-- 1.b
tiempoViaje :: Planeta -> Planeta -> Float -> Float
tiempoViaje planetaA planetaB = (/) (distanciaPlanetas planetaA planetaB)

-- 2
aumentarEdad :: Astronauta -> Float -> Astronauta
aumentarEdad astronauta anios = (UnAstronauta (nombre astronauta) ((+anios).edad $ astronauta) (planeta astronauta) )

pasarTiempo :: Astronauta -> Float -> Astronauta
pasarTiempo astronauta anios = aumentarEdad astronauta ((tiempo.planeta $ astronauta) $ anios) 

-- 3
type Nave = Planeta -> Planeta -> Float
naveVieja :: Int -> Nave
naveVieja oxigeno planetaA planetaB
  | oxigeno < 6 = tiempoViaje planetaA planetaB 10
  | otherwise = tiempoViaje planetaA planetaB 7

naveFuturista :: Nave 
naveFuturista _ _ = 0

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta planetaNuevo astronauta = (UnAstronauta (nombre astronauta) (edad astronauta) planetaNuevo )

viajar :: Nave -> Planeta -> Astronauta -> Astronauta
viajar nave planetaDestino astronauta
  | tiempoNave == 0 = cambiarPlaneta planetaDestino astronauta
  | otherwise = cambiarPlaneta planetaDestino.pasarTiempo astronauta.(/ tiempoNave).distanciaPlanetas planetaDestino $ planetaOrigen
    where
        planetaOrigen = planeta astronauta
        tiempoNave = (nave planetaOrigen planetaDestino)

-- 4.a
viajeAstronautas :: Nave -> Planeta -> [Astronauta] -> [Astronauta]
viajeAstronautas nave planetaDestino= map (viajar nave planetaDestino)

rescate :: [Astronauta] -> Nave -> Astronauta -> [Astronauta]
rescate rescatistas nave astronauta = viajeAstronautas nave planetaOrigen.flip (:) (viajeAstronautas nave planetaDestino rescatistas).flip pasarTiempo tiempoIda $ astronauta
    where
        planetaDestino = planeta astronauta
        planetaOrigen = planeta.head $ rescatistas
        tiempoIda = (nave planetaOrigen planetaDestino)

-- 4.b
puedeSerRescatado :: [Astronauta] -> Nave -> Astronauta -> Bool
puedeSerRescatado rescatistas nave astronauta = not.any (>90).map edad.rescate rescatistas nave $ astronauta

rescatesViables :: [Astronauta] -> Nave -> [Astronauta] -> [String]
rescatesViables rescatistas nave varados = map nombre.filter (puedeSerRescatado rescatistas nave) $ varados

-- 5
f :: (Ord b) => (b -> d -> b) -> b -> (Int -> d -> Bool) -> [d] -> Bool
f a b c = any ((> b).a b).filter (c 10)
