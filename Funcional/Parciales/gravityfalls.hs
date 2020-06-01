{------------------------
----  PRIMERA PARTE  ----
------------------------}

--[1]
type Item = String
type Requerimientos = Persona -> Bool

soplador :: Item
soplador = "soplador de hojas"

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
    peligrosidad :: Int,
    requerimientos :: [Requerimientos]
}

esSiempredetras :: Int -> Bool
esSiempredetras = (==) tipoSiempredetras

esGnomo :: Int -> Bool
esGnomo = (==) tipoGnomos

esFantasma :: Int -> Bool
esFantasma = (==) tipoFantasma

--[2]
crearCriatura :: Int -> Int -> [Requerimientos] -> Criatura
crearCriatura tipoCriatura parametro requerimientosCriatura = Criatura {
    tipo = tipoCriatura,
    peligrosidad = setPeligrosidad tipoCriatura parametro,
    requerimientos = requerimientosCriatura
}

setPeligrosidad :: Int -> Int -> Int
setPeligrosidad tipoCriatura parametro
  | esGnomo tipoCriatura = 2 ^ parametro
  | esFantasma tipoCriatura = parametro * 20
  | esSiempredetras tipoCriatura = 0
  | otherwise = undefined

darExperiencia :: Int -> Persona -> Persona
darExperiencia valor persona = persona { experiencia = (+) valor.experiencia $ persona}

tieneItem :: Item -> Persona -> Bool
tieneItem item = elem item.items

leGana :: Persona -> Criatura -> Bool
leGana persona criatura
  | esSiempredetras (tipo criatura) = False
  | otherwise = all (==True).map ($ persona).requerimientos $ criatura

calcularExperiencia :: Persona -> Criatura -> Int
calcularExperiencia persona criatura
  | leGana persona criatura = peligrosidad criatura
  | otherwise = 1 

enfrentar :: Persona -> Criatura -> Persona
enfrentar persona criatura = darExperiencia (calcularExperiencia persona criatura) persona

--[3]
enfrentarGrupo :: Persona -> [Criatura] -> Persona
enfrentarGrupo persona criaturas = foldl (enfrentar) persona criaturas

diferenciaExp :: Persona -> Persona -> Int
diferenciaExp persona1 = (-) (experiencia persona1).experiencia 

{-
Consultas:
Main> diferenciaExp (enfrentarGrupo menorDe13 grupoCriaturas) menorDe13
1105
Main> diferenciaExp (enfrentarGrupo perdedor grupoCriaturas) perdedor
4
Main> diferenciaExp (enfrentarGrupo cazaFantasma1 grupoCriaturas) cazaFantasma1
23
-}

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
fantasma1 :: Criatura
fantasma1 = crearCriatura tipoFantasma 1 [(>10).experiencia]

fantasma3 :: Criatura
fantasma3 = crearCriatura tipoFantasma 3 [tieneItem "disfraz de oveja", (<13).edad]

gnomos :: Criatura
gnomos = crearCriatura tipoGnomos 10 [tieneItem soplador]

siempreDetras :: Criatura
siempreDetras = crearCriatura tipoSiempredetras 0 []

grupoCriaturas :: [Criatura]
grupoCriaturas = [siempreDetras, gnomos, fantasma3, fantasma1]

mataGnomos :: Persona
mataGnomos = Persona {
    edad = 12,
    items = [soplador],
    experiencia = 0
}

menorDe13 :: Persona
menorDe13 = Persona {
    edad = 12,
    items = [soplador, "disfraz de oveja"],
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