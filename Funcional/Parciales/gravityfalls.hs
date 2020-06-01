{------------------------
----  PRIMERA PARTE  ----
------------------------}

--[1]
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

--[2]
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

calcularExperiencia :: Persona -> Criatura -> Int
calcularExperiencia persona criatura
  | leGana persona criatura = peligrosidad criatura
  | otherwise = 1 

enfrentar :: Criatura -> Persona -> Persona
enfrentar criatura persona = darExperiencia (calcularExperiencia persona criatura) persona

--[3]
calcularExperienciaGrupo :: [Criatura] -> Persona -> Int
calcularExperienciaGrupo criaturas persona = foldl1 (+).map (calcularExperiencia persona) $ criaturas

{-
Consultas:
Main> calcularExperienciaGrupo grupoCriaturas menorDe13
1105
Main> calcularExperienciaGrupo grupoCriaturas perdedor
4
Main> calcularExperienciaGrupo grupoCriaturas cazaFantasma1
23
-}

enfrentarGrupo :: [Criatura] -> Persona -> Persona
enfrentarGrupo criaturas persona = darExperiencia (calcularExperienciaGrupo criaturas persona) persona

-- ALTERNATIVAMENTE:
enfrentar' :: Criatura -> Persona -> Persona
enfrentar' criatura persona
  | leGana persona criatura = darExperiencia (peligrosidad criatura) persona
  | otherwise = darExperiencia 1 persona

enfrentarGrupo' :: [Criatura] -> Persona -> Persona
enfrentarGrupo' [] persona = persona
enfrentarGrupo' (c:cs) persona = enfrentarGrupo' cs.enfrentar' c $ persona

{------------------------
----  SEGUNDA PARTE  ----
------------------------}

--[1]
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = []
zipWithIf f criterio (x:xs) (y:ys)
  | criterio y = (:) (f x y) (zipWithIf f criterio xs ys)
  | otherwise = y : zipWithIf f criterio (x:xs) (ys)

--[2.a]
abecedarioDesde :: Char -> [Char] -- No se usa...
abecedarioDesde inicio = (++) [inicio..'z'].init $ ['a'..inicio]

--[2.b]
abecedarioDesdeHasta :: Char -> Char -> [Char]
abecedarioDesdeHasta inicio final
  | inicio <= final = [inicio..final]
  | otherwise = (++) [inicio..'z'] ['a'..final]

esLetra :: Char -> Bool
esLetra letra = elem letra ['a'..'z']

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra clave = last.flip take ['a'..'z'].length.abecedarioDesdeHasta clave

--[2.c]
cesar :: Char -> String -> String
cesar clave texto = zipWithIf (desencriptarLetra) (esLetra) (repeat clave) texto

--[2.d]
rotar :: String -> [String]
rotar texto = map (`cesar` texto) ['a'..'z']

{-
Main> rotar "jrzel zrfaxal!"
["jrzel zrfaxal!","iqydk yqezwzk!","hpxcj xpdyvyj!","gowbi wocxuxi!","fnvah vnbwtwh!","emuzg umavsvg!","dltyf tlzuruf!",
"cksxe skytqte!","bjrwd rjxspsd!","aiqvc qiwrorc!","zhpub phvqnqb!","ygota ogupmpa!","xfnsz nftoloz!","wemry mesnkny!",
"vdlqx ldrmjmx!","uckpw kcqlilw!","tbjov jbpkhkv!","sainu iaojgju!","rzhmt hznifit!","qygls gymhehs!","pxfkr fxlgdgr!",
"owejq ewkfcfq!","nvdip dvjebep!","mucho cuidado!","ltbgn bthczcn!","ksafm asgbybm!"]
-}

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

grupoCriaturas :: [Criatura]
grupoCriaturas = [siempreDetras, gnomos, fantasma3, fantasma1]

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