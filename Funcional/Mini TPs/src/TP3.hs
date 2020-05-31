module TP3 where
import Cafe

ml_agua_cafe = 1000

ml_agua_frapu = 80
ml_vaso_frapu = 400
ml_leche_frapu = 120
cant_hielo_frapu = 6
duracion_liq_frapu = 60

armarCafe :: Vaso -> Gramos -> Cafe
armarCafe vaso gramos = (servirEnVaso vaso).(prepararCafe ml_agua_cafe).molerGranos $ gramos

armarFrapu :: Gramos -> Cafe
armarFrapu = (servirEnVaso ml_vaso_frapu).(licuar duracion_liq_frapu ml_leche_frapu).(agregarHielo cant_hielo_frapu).(prepararCafe ml_agua_frapu).molerGranos 