import Test.Hspec
import TP

main :: IO ()
main = hspec $ do
  describe "El código base anda y el TP compila" $ do
    it "Las funciones se pueden mostrar como <function>" $ do
      show id `shouldBe` "<function>"
    it "Ordenar por ... ordena" $ do
      ordenarPor id [4,3,6,1] `shouldBe` [1,3,4,6]

  describe "Primer Punto" $ do
    it "Felicidad humano de prueba es 50" $ do
      (felicidad iluso) `shouldBe` 100
    it "Reconocimiento humano de prueba es 100" $ do
      (reconocimiento iluso) `shouldBe` 50
    it "Paz Mundial debería multiplicar por 20 la felicidad del humano" $ do
      (felicidad.pazMundial) iluso `shouldBe` ((*20).felicidad) iluso

  describe "Segundo Punto" $ do
    it "Para el humano de ejemplo, la espiritualidad del deseo de recibirse de la carrera \"Medicina\" sería 226." $ do
      espiritualidad iluso (recibirse "Medicina") `shouldBe` 226

  describe "Tercer Punto" $ do
    it "Para el humano de ejemplo, luego de cumplir todos sus deseos en el orden indicado en el punto 1 no sería más feliz, porque al volverse famoso quedaría con felicidad 50" $ do
      deseosMejoranFelicidad iluso `shouldBe` False

  describe "Cuarto Punto" $ do
    it "Deseos más terrenales del humano de prueba tienen espiritualidades de [-1050,139]" $ do
      map (espiritualidad iluso) (deseos (ordenarDeseos ordenTerrenal iluso)) `shouldBe` [-1050,139]
    it "La mejor versión del humano de prueba sería la resultante de cumplir con su deseo de paz mundial." $ do
      (felicidad.mejorVersionDeHumano) iluso `shouldBe` (felicidad.pazMundial) iluso

  describe "Quinto Punto" $ do
    it "Humano de prueba es deudor de Abaddon" $ do
      esDeudorBonus iluso abaddon `shouldBe` True
    it "Humano de prueba no deudor de Lucifer" $ do
      esDeudorBonus estudiante lucifer `shouldBe` False
    it "Juancito es deudor de Lucifer" $ do
      esDeudorBonus (Humano "Juancito" 1 1 []) lucifer `shouldBe` True
    it "Estudiante anonimo no es deudor de nadie" $ do
      esDeudorBonus estudiante abaddon `shouldBe` False

  describe "Sexto Punto" $ do
    it "Un demonio le cumpliría el deseo de ser famoso al humano de prueba, haciendolo deudor" $ do
      ((\ (x,y) -> (felicidad x, reconocimiento x, (any (== (nombre x)).deudores) y)).ayudarSiLeConviene iluso) lucifer `shouldBe` ((felicidad.serFamoso) iluso, (reconocimiento.serFamoso) iluso, True)
    it "Un demonio no cumple deseo si el humano ya es deudor" $ do
      ((\ (x,y) -> (felicidad x, reconocimiento x, (any (== (nombre x)).deudores) y)).ayudarSiLeConviene iluso) abaddon `shouldBe` (felicidad iluso, reconocimiento iluso, False)
    it "Un demonio no cumple deseos si el humano no tiene deseos terrenales" $ do
      ((\ (x,y) -> (felicidad x, reconocimiento x, (any (== (nombre x)).deudores) y)).ayudarSiLeConviene (Humano "" 5 10 [pazMundial])) abaddon `shouldBe` (10, 5, False)