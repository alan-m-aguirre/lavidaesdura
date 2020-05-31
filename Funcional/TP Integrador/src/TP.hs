module TP where
import Text.Show.Functions

----------------------
-- Código inicial
----------------------

ordenarPor :: Ord a => (b -> a) -> [b] -> [b]
ordenarPor ponderacion =
  foldl (\ordenada elemento -> filter ((< ponderacion elemento).ponderacion) ordenada
                                  ++ [elemento] ++ filter ((>= ponderacion elemento).ponderacion) ordenada) []

data Demonio = Demonio {
    deudores :: [String],
    subordinados :: [Demonio]
} deriving (Show, Eq)

----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
----------------------------------------------

------------------
-- Primer Punto -- 
------------------
felicidadPazMundial = 20
felicidadRecibirse = 250
felicidadFamoso = 50
multiplicadorReconocimiento = 3
reconocimientoFamoso = 1000

type Deseo = Humano -> Humano

data Humano = Humano {
    nombre :: String,
    reconocimiento :: Int,
    felicidad :: Int,
    deseos :: [Deseo]
} deriving (Show)

--Humano de Prueba
iluso :: Humano
iluso = Humano {
  nombre = "Iluso Soniador",
  reconocimiento = 50,
  felicidad = 100,
  deseos = [pazMundial, recibirse "Ingeniería en Sistemas de Información", recibirse "Medicina", serFamoso]
}

setear :: Int -> Int -> Int
setear a b = a

cambiarFelicidad :: (Int -> Int -> Int) -> Int -> Humano -> Humano
cambiarFelicidad f val humano = humano { felicidad = f val.felicidad $ humano } 

cambiarReconocimiento :: (Int -> Int -> Int) -> Int -> Humano -> Humano
cambiarReconocimiento f val humano = humano { reconocimiento = f val.reconocimiento $ humano } 
  
pazMundial :: Deseo
pazMundial = cambiarFelicidad (*) felicidadPazMundial

recibirse :: String -> Deseo
recibirse carrera = cambiarReconocimiento (+) ((*multiplicadorReconocimiento).length $ carrera).cambiarFelicidad (+) felicidadRecibirse

serFamoso :: Deseo
serFamoso = cambiarFelicidad setear felicidadFamoso.cambiarReconocimiento (+) reconocimientoFamoso

-------------------
-- Segundo Punto -- 
-------------------
diferenciaSegun :: (Humano -> Int) -> Humano -> Humano -> Int
diferenciaSegun f humano1 = (-) (f humano1).f

diferenciaFelicidad :: Humano -> Humano -> Int
--Devuelve la diferencia entre la felicidad de dos humanos
diferenciaFelicidad = diferenciaSegun felicidad

diferenciaReconocimiento :: Humano -> Humano -> Int
--Devuelve la diferencia entre el reconocimiento de dos humanos
diferenciaReconocimiento = diferenciaSegun reconocimiento

espiritualidad :: Humano -> Deseo -> Int
espiritualidad humano deseo = diferenciaFelicidad (deseo humano) humano - diferenciaReconocimiento (deseo humano) humano

------------------
-- Tercer Punto -- 
------------------
cumplirDeseo :: Humano -> Deseo -> Humano
--Aplica un deseo a un humano
cumplirDeseo humano deseo = deseo humano

cumplirTodosDeseos :: Deseo 
--Cumple todos los deseos de un humano
cumplirTodosDeseos humano = foldl cumplirDeseo humano (deseos humano)

deseosMejoranFelicidad :: Humano -> Bool
--Comprueba si cumplir los deseos de un humano devuelve a un humano con mayor felicidad.
deseosMejoranFelicidad humano = (>0).diferenciaFelicidad (cumplirTodosDeseos humano) $ humano

------------------
-- Cuarto Punto -- 
------------------

--4.a)
espiritualidadMinima = 150

esTerrenal :: Humano -> Deseo -> Bool
esTerrenal humano = (< espiritualidadMinima).espiritualidad humano

ordenTerrenal :: Humano -> [Deseo]
ordenTerrenal humano = filter (esTerrenal humano).ordenarPor (espiritualidad humano).deseos $ humano

ordenarDeseos :: (Humano -> [Deseo]) -> Humano -> Humano
ordenarDeseos fOrden humano = Humano {
  nombre = nombre humano,
  reconocimiento = reconocimiento humano,
  felicidad = felicidad humano,
  deseos = fOrden humano
}

--4.b)
ordenMejorDeseo :: Humano -> [Deseo]
ordenMejorDeseo humano = reverse.ordenarPor (estadoDeHumanoPostDeseo humano).deseos $ humano

estadoDeHumano :: Humano -> Int
estadoDeHumano humano = (+) (felicidad humano) (reconocimiento humano)

estadoDeHumanoPostDeseo :: Humano -> Deseo -> Int
estadoDeHumanoPostDeseo humano deseo = estadoDeHumano (deseo humano)

mejorVersionDeHumano :: Humano -> Humano
mejorVersionDeHumano humano = (head.ordenMejorDeseo $ humano) humano

------------------
-- Quinto Punto -- 
------------------

--5.a) 
esDeudorDirecto :: Humano -> Demonio -> Bool
esDeudorDirecto humano demonio = elem (nombre humano) (deudores demonio)

esDeudorIndirecto :: Humano -> [Demonio] -> Bool
esDeudorIndirecto humano [] = False
esDeudorIndirecto humano (x:xs) = esDeudorDirecto humano x || esDeudorIndirecto humano xs || esDeudorIndirecto humano (subordinados x)

esDeudor :: Humano -> Demonio -> Bool
esDeudor humano demonio = esDeudorDirecto humano demonio || esDeudorIndirecto humano (subordinados demonio)

--5.b)
cadenaDeMando :: Demonio -> [Demonio]
cadenaDeMando demonio = ((:) demonio).(concatMap cadenaDeMando) $ (subordinados demonio)

esDeudorBonus :: Humano -> Demonio -> Bool
esDeudorBonus humano demonio = any (== (nombre humano)).concatMap deudores.cadenaDeMando $ demonio

{- 
 5.c) Gracias a la transparencia referencial, Haskell usa evaluación perezosa (lazy), entonces puede encontrar la solución
      a un poblema antes de tener que evaluar la totalidad de una expresion. En este caso, en esDeudorIndirecto (función
      recursiva), se simplifica la expresión compuesta por potencialmente infinitos (||) y va a evaluandolos "de afuera hacia
      adentro". Una vez que encuentra la solución, por reglas lógicas, se sabe que un 'True' en una serie infinita de || va a
      devolver 'True', entonces termina de evaluar. La ventaja de Haskell es que no solo aplica esto para operadores lógicos,
      sino que tiene un alcance mayor: se puede usar con funciones que limiten el dominio de algo infinito, por ejemplo,
      funciones como take, any, all, head, etc., dependiendo de cómo se implementen.

      En nuestra función esDeudorBonus, a pesar de estar evaluando la lista generada por cadenaDeMando que potencialmente
      podría ser infinita, una vez que encuentra un deudor con el nombre del humano (gracias al any) ya sabe que es cierto,
      entonces deja de evaluar y devuelve el resultado.

      Cabe destacar que cuando el resultado de nuestras funciones es 'False', Haskell seguirá evaluando cada expresión: entra
      en un loop infinito ya que nunca encontraría el resultado que le permita cortar con subsiguientes evaluaciones.
      
      En general, si no puede llegar a este tipo de solución, si no tiene esa condición, va a seguir evaluando infinitamente.
      Por ejemplo:      all (>0) [1..]      en una lista infinita de numeros positivos, nunca se va a encontrar un número que
      no cumpla con la condición (<=0), por lo tanto el all se mantiene 'True' y sigue evaluando para siempre.
-}

-----------------
-- Sexto Punto -- 
-----------------
cumplirDeseoMasTerrenal :: Humano -> Humano
cumplirDeseoMasTerrenal humano = (head.ordenTerrenal $ humano) humano

agregarDeudor :: Humano -> Demonio -> Demonio
agregarDeudor humano demonio = Demonio {
  deudores = (:) (nombre humano) (deudores demonio),
  subordinados = subordinados demonio
}

tieneDeseosTerrenales :: Humano -> Bool
tieneDeseosTerrenales = (>0).length.ordenTerrenal

noLeConviene :: Humano -> Demonio -> Bool
noLeConviene humano demonio = (esDeudor humano demonio) || (not.tieneDeseosTerrenales $ humano)

ayudarSiLeConviene :: Humano -> Demonio -> (Humano, Demonio)
ayudarSiLeConviene humano demonio
  | noLeConviene humano demonio = (humano,demonio)
  | otherwise = (cumplirDeseoMasTerrenal humano, agregarDeudor humano demonio)

-----------------------------------------------
-- Extra: Demonios y Otros Humanos de Prueba -- 
-----------------------------------------------
abaddon :: Demonio
abaddon = Demonio {
    deudores = ["Aleister Crowley"],
    subordinados = [satanas]
}

satanas :: Demonio
satanas = Demonio {
    deudores = ["Carlos Menem", "Dorian Gray"],
    subordinados = [beelzebub]
} 

beelzebub :: Demonio
beelzebub = Demonio {
    deudores = [nombre iluso],
    subordinados = [lucifer]
} 

lucifer :: Demonio
lucifer = Demonio {
    deudores = ["Juancito"],
    subordinados = []
} 

estudiante :: Humano
estudiante = Humano {
  nombre = "Estudiante Anónimo",
  reconocimiento = 0,
  felicidad = 42,
  deseos = [recibirse "Ingeniería en Sistemas de Información"]
}