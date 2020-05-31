module TP2 where
import Video

minutosPorHora :: Int
minutosPorHora = 60

esMasLargo :: Video -> Video -> Bool
esMasLargo video1 video2 = duracion video1 > duracion video2

duracionEnMinutos :: Video -> Int
duracionEnMinutos video = pasarAMinutos (duracion video)

pasarAMinutos :: Duracion -> Int
pasarAMinutos (h, m, _) = h * minutosPorHora + m

promedioVisualizacion :: Int -> Duracion -> Int
promedioVisualizacion vistas duracion = promedioInt (pasarAMinutos duracion) vistas

promedioInt :: Int -> Int -> Int
promedioInt valor total = div (valor * 100) total

porcentajeDeReproduccionPromedio :: Int -> Duracion -> Video -> Int
porcentajeDeReproduccionPromedio vistas duracionTotal video =  div (promedioVisualizacion vistas duracionTotal) (duracionEnMinutos video)