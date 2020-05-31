module TP1 where

esMes :: Int -> Bool
esMes mes = mes >= 1 && mes <= 12

esMultiploDeTres :: Int -> Bool
esMultiploDeTres num = mod num 3 == 0

hayCambioDeEstacion :: Int -> Bool
hayCambioDeEstacion mes = esMes mes && esMultiploDeTres mes

mesSiguiente :: Int -> Int
mesSiguiente 12 = 1
mesSiguiente mes = mes + 1

estacionSegunTrimestre :: Int -> String
estacionSegunTrimestre 0 = "verano"
estacionSegunTrimestre 1 = "otoño"
estacionSegunTrimestre 2 = "invierno"
estacionSegunTrimestre 3 = "primavera"

estacion :: Int -> String
estacion mes
  | hayCambioDeEstacion mes = estacion (mes-1) ++ "/" ++ (estacion.mesSiguiente)  mes
  | esMes mes = estacionSegunTrimestre (div mes 3)