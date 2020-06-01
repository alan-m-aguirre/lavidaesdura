{------------------------
----  PRIMERA PARTE  ----
------------------------}
type Item = String
type Requerimientos = Persona -> Bool

tipoSiempredetras :: Int
tipoSiempredetras = 1

tipoGnomos :: Int
tipoGnomos = 2

tipoFantasma :: Int
tipoFantasma = 3

data Persona = Persona {
    edad :: Int,
    items :: [Item],
    experiencia :: Int
} deriving (Show)

data Criatura = Criatura {
    tipo :: Int,
    cantidad :: Int,
    nivel :: Int,
    requerimientos :: [Requerimientos]
}

esSiempredetras :: Criatura -> Bool
esSiempredetras = (==) tipoSiempredetras.tipo

esGnomo :: Criatura -> Bool
esGnomo = (==) tipoGnomos.tipo

esFantasma :: Criatura -> Bool
esFantasma = (==) tipoFantasma.tipo

peligrosidad :: Criatura -> Int
peligrosidad criatura
  | esGnomo criatura = (2^).cantidad $ criatura
  | esFantasma criatura = (*20).nivel $ criatura
  | esSiempredetras criatura = 0
  | otherwise = undefined

darExperiencia :: Int -> Persona -> Persona
darExperiencia valor persona = persona { experiencia = (+) valor.experiencia $ persona}

tieneItem :: Item -> Persona -> Bool
tieneItem item = elem item.items

leGana :: Persona -> Criatura -> Bool
leGana persona criatura
  | esSiempredetras criatura = False
  | esGnomo criatura = tieneItem "soplador de hojas" persona
  | otherwise = all (==True).map ($ persona).requerimientos $ criatura

enfrentar :: Persona -> Criatura -> Persona
enfrentar persona criatura
  | leGana persona criatura = darExperiencia (peligrosidad criatura) persona
  | otherwise = darExperiencia 1 persona

--Consultas:
--enfrentar mataGnomos fantasma1
--enfrentar mataGnomos gnomos
--enfrentar perdedor gnomos
--enfrentar perdedor fantasma3
--enfrentar menorDe13 fantasma3
--enfrentar menorDe13 fantasma1
--enfrentar cazaFantasma1 fantasma1
--enfrentar cazaFantasma1 fantasma3

{------------------------
----  SEGUNDA PARTE  ----
------------------------}
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = []
zipWithIf f criterio (x:xs) (y:ys)
  | criterio y = (:) (f x y) (zipWithIf f criterio xs ys)
  | otherwise = y : zipWithIf f criterio (x:xs) (ys)

abecedarioDesde :: Char -> [Char] -- No se usa...
abecedarioDesde inicio = (++) [inicio..'z'].init $ ['a'..inicio]

abecedarioDesdeHasta :: Char -> Char -> [Char]
abecedarioDesdeHasta inicio final
  | inicio <= final = [inicio..final]
  | otherwise = (++) [inicio..'z'] ['a'..final]

esLetra :: Char -> Bool
esLetra letra = elem letra ['a'..'z']

rotar :: String -> [String]
rotar texto = map (`cesar` texto) ['a'..'z']

desencriptarLetra :: Char -> Char -> Char
--desencriptarLetra clave = (!!) ['a'..'z'].(+(-1)).length.abecedarioDesdeHasta clave
desencriptarLetra clave = last.flip take ['a'..'z'].length.abecedarioDesdeHasta clave

cesar :: Char -> String -> String
cesar clave texto = zipWithIf (desencriptarLetra) (esLetra) (repeat clave) texto

{------------------------
----      BONUS      ----
------------------------}
vigenere :: String -> String -> String
vigenere clave texto = zipWithIf (desencriptarLetra) (esLetra) (cycle clave) texto

{------------------------
----SUJETOS DE PRUEBA----
------------------------}
criaturaBase :: Criatura
criaturaBase = Criatura {
    tipo = 0,
    cantidad = 1,
    nivel = 0,
    requerimientos = []
}

fantasma1 :: Criatura
fantasma1 = criaturaBase {
    tipo = tipoFantasma,
    nivel = 1,
    requerimientos = [(>10).experiencia]
}

fantasma3 :: Criatura
fantasma3 = criaturaBase {
    tipo = tipoFantasma,
    nivel = 3,
    requerimientos = [tieneItem "disfraz de oveja", (<13).edad]
}

siempreDetras :: Criatura
siempreDetras = criaturaBase {
    tipo = tipoSiempredetras
}

gnomos :: Criatura
gnomos = criaturaBase {
    tipo = tipoGnomos,
    cantidad = 10
}

mataGnomos :: Persona
mataGnomos = Persona {
    edad = 12,
    items = ["soplador de hojas"],
    experiencia = 0
}

menorDe13 :: Persona
menorDe13 = Persona {
    edad = 12,
    items = ["soplador de hojas", "disfraz de oveja"],
    experiencia = 11
}

perdedor :: Persona
perdedor = Persona {
    edad = 15,
    items = [],
    experiencia = 0
}

cazaFantasma1 :: Persona
cazaFantasma1 = Persona {
    edad = 15,
    items = [],
    experiencia = 50
}

{------------------------
----   CODIGO  FEO   ----
----     IGNORAR     ----
------------------------}
truncarAbecedario :: [Char] -> Char -> [Char] --Mejor forma de hacer esto? hecho: abecedarioDesdeHasta
truncarAbecedario [] _ = []
truncarAbecedario (a:az) letra
  | a /= letra = a : truncarAbecedario az letra
  | otherwise = [a]

desencriptarLetra' :: Char -> Char -> Char
desencriptarLetra' clave input
  | esLetra input = (!!) ['a'..'z'].(+(-1)).length.truncarAbecedario (abecedarioDesde clave) $ input
  | otherwise = input

cesar' :: Char -> String -> String
cesar' clave texto = map (desencriptarLetra clave) texto --prefiero usar map antes que ZipWithIf.

repetirClave :: String -> String
repetirClave clave = (++) clave.repetirClave $ clave

generarTextoClave :: String -> String -> String
generarTextoClave clave texto = zipWithIf (\ x _ -> x) (esLetra) (repetirClave clave) texto