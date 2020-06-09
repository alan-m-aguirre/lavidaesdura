data Elemento = UnElemento { 
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje)
}

data Personaje = UnPersonaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}

-- 1.a
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje { anioPresente = anio }

-- 1.b
cambiarSalud :: (Float -> Float) -> Personaje -> Personaje
cambiarSalud f personaje = personaje { salud = f.salud $ personaje }

meditar :: Personaje -> Personaje
meditar personaje = cambiarSalud (+ recupera) personaje
    where
        recupera = (/2).salud $ personaje

-- 1.c
causarDanio :: Float -> Personaje -> Personaje
causarDanio danio personaje
  | danio > salud personaje = cambiarSalud (*0) personaje
  | otherwise = cambiarSalud (+ (negate danio)) personaje

-- 2.a
maldad :: String
maldad = "Maldad"

esElementoMalvado :: Elemento -> Bool
esElementoMalvado = (==maldad).tipo

esMalvado :: Personaje -> Bool
esMalvado = any (==True).map esElementoMalvado.elementos

-- 2.b
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = (-) saludIncial saludFinal
    where
        saludIncial = salud personaje
        saludFinal = salud.ataque elemento $ personaje

-- 2.c
puedeMatarDeUnGolpe :: Personaje -> Personaje -> Bool
puedeMatarDeUnGolpe victima atacante = any (> salud victima).map (danioQueProduce victima).elementos $ atacante

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (puedeMatarDeUnGolpe personaje) $ enemigos

-- 3.a
concentracion :: Int -> Elemento
concentracion nivel = UnElemento {
    tipo = "Magia",
    ataque = id,
    defensa = meditarVeces nivel
}

meditarVeces :: Int -> (Personaje -> Personaje)
meditarVeces nivel = foldl1 (.).take nivel.repeat $ meditar

-- 3.b
esbirroMalvado :: Elemento
esbirroMalvado = UnElemento {
    tipo = maldad,
    defensa = id,
    ataque = causarDanio 1
}

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = take cantidad.repeat $ esbirroMalvado

-- 3.c
katanaMagica :: Elemento
katanaMagica = UnElemento {
    tipo = "Magia",
    ataque = causarDanio 1000,
    defensa = id
}

jack :: Personaje
jack = UnPersonaje {
    nombre = "Jack",
    elementos = [concentracion 3, katanaMagica],
    anioPresente = 200,
    salud = 300
}

-- 3.d
portal :: Int -> Elemento 
portal anio = UnElemento {
    tipo = "Magia",
    ataque = mandarAlAnio anio,
    defensa = id
}

aku :: Int -> Float -> Personaje
aku anio cantSalud = UnPersonaje {
    nombre = "Aku",
    salud = cantSalud,
    elementos = [concentracion 4] ++ (esbirrosMalvados (anio*100)) ++ [portal (anio+2800)],
    anioPresente = anio
}

-- 4 
foldearElementosPorEfecto :: (Elemento -> (Personaje-> Personaje)) -> Personaje -> (Personaje-> Personaje)
foldearElementosPorEfecto efecto personaje = foldl1 (.).reverse.map efecto.elementos $ personaje

atacar :: Personaje -> Personaje -> (Personaje, Personaje)
atacar atacante defensor = (efectoDefensa $ atacante, efectoAtaque $ defensor)
    where
        efectoDefensa = foldearElementosPorEfecto defensa atacante
        efectoAtaque = foldearElementosPorEfecto ataque atacante

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
  | (==0).salud $ atacante = (defensor, atacante)
  | otherwise = luchar (snd personajesPostAtaque) (fst personajesPostAtaque)
    where
        personajesPostAtaque = atacar atacante defensor

-- 5

{- 

f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

f :: x -> y -> z -> [] -> []
f :: x -> (Int -> a) -> a -> [] -> []
f :: (a -> b -> (c,c)) -> (Int -> a) -> a -> [b] -> [c]
f :: (Eq a) => (a -> b -> (c,c)) -> (Int -> a) -> a -> [b] -> [c]

-}