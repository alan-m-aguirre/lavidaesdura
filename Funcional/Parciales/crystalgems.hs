
data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> Situacion -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> Situacion -> Situacion
reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

{------------------------
----  PRIMER PUNTO   ----
------------------------}

--a.
modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto f aspecto = aspecto { grado = f.grado $ aspecto }

--b.
mejorAspectoEnSituacion :: Situacion -> Aspecto -> Bool
mejorAspectoEnSituacion situacion aspecto = mejorAspecto aspecto.buscarAspectoDeTipo (tipoDeAspecto aspecto) $ situacion

mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion situacion1 situacion2 = all (==True).map (mejorAspectoEnSituacion situacion2) $ situacion1

--c.
modificarSituacion :: Aspecto -> (Float -> Float) -> Situacion -> Situacion
modificarSituacion aspectoBuscado f situacion = flip reemplazarAspecto situacion.modificarAspecto f.flip buscarAspectoDeTipo situacion.tipoDeAspecto $ aspectoBuscado

{------------------------
----  SEGUNDO PUNTO  ----
------------------------}
type Personalidad = (Situacion -> Situacion)

data Gema = Gema {
    nombre :: String,
    fuerza :: Int,
    personalidad :: Personalidad
}

test :: Situacion
test = [(UnAspecto "incertidumbre" 100),(UnAspecto "tension" 100),(UnAspecto "peligro" 100)]

test2 :: Situacion
test2 = [(UnAspecto "incertidumbre" 50),(UnAspecto "peligro" 50),(UnAspecto "tension" 50)]

test3 :: Situacion
test3 = [(UnAspecto "incertidumbre" 50)]

vidente :: Personalidad
vidente = modificarSituacion (UnAspecto "tension" 0) (+(-10)).modificarSituacion (UnAspecto "incertidumbre" 0) (/2)

relajada :: Float -> Personalidad
relajada valor = modificarSituacion (UnAspecto "peligro" 0) (+valor).modificarSituacion (UnAspecto "tension" 0) (+(-30)) 

gemaDescuidada :: Gema
gemaDescuidada = Gema {
  nombre = "Descuidada",
  fuerza = 1,
  personalidad = relajada 10
}

gemaVidente :: Gema
gemaVidente = Gema {
  nombre = "Vidente",
  fuerza = 1,
  personalidad = vidente
}

{------------------------
----  TERCER  PUNTO  ----
------------------------}
leGana :: Gema -> Gema -> Situacion -> Bool
leGana gema1 gema2 situacion = (fuerza gema1) > (fuerza gema2) && mejorSituacion (personalidad gema1 $ situacion) (personalidad gema2 $ situacion)

{------------------------
----  CUARTO  PUNTO  ----
------------------------}
reemplazarNombre :: String -> String -> String
reemplazarNombre nombre1 nombre2
  | nombre1 == nombre2 = nombre1
  | otherwise = nombre1 ++ nombre2

bajarAspectoPorFusion :: Aspecto -> Aspecto
bajarAspectoPorFusion aspecto = aspecto { grado = (+(-10)).grado $ aspecto}

efectoFusion :: Situacion -> Situacion
efectoFusion = map bajarAspectoPorFusion

nuevaPersonalidad :: Personalidad -> Personalidad -> Personalidad
nuevaPersonalidad personalidad1 personalidad2 = personalidad2.personalidad1.efectoFusion

compatibilidad :: Gema -> Gema -> Situacion -> Bool
compatibilidad gema1 gema2 situacion = mejorSituacion (nuevaPersonalidad (personalidad gema1) (personalidad gema2) $ situacion) (personalidad gema2.personalidad gema1 $ situacion)

nuevaFuerza ::  Situacion-> Gema -> Gema -> Int
nuevaFuerza situacion gema1 gema2
  | compatibilidad gema1 gema2 situacion = (*10).(+) (fuerza gema2).fuerza $ gema1
  | fuerza gema1 > fuerza gema2 = (*7).fuerza $ gema1
  | otherwise = (*7).fuerza $ gema2

fusion :: Situacion -> Gema -> Gema -> Gema
fusion situacion (Gema nombre1 fuerza1 personalidad1) (Gema nombre2 fuerza2 personalidad2) = Gema {
  nombre = reemplazarNombre nombre1 nombre2,
  personalidad = nuevaPersonalidad personalidad1 personalidad2,
  fuerza = nuevaFuerza situacion (Gema nombre1 fuerza1 personalidad1) (Gema nombre2 fuerza2 personalidad2)
}

{------------------------
----  QUINTO  PUNTO  ----
------------------------}
fusionGrupal :: Situacion -> [Gema] -> Gema
fusionGrupal situacion = foldr1 (fusion situacion)