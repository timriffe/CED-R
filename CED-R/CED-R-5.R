
# Author: triffe
###############################################################################

# ------------------------------------------------------------------
# de Amand:
# ------------------------------------------------------------------
##### descomponer diferencias entre TBM

DesComTBM <- function (def1, pob1, def2, pob2){
    
    tasas1 <- c(def1/pob1)
    tasas2 <- c(def2/pob2) 
    
    TBM1 <- c(sum(def1)/sum(pob1))
    TBM2 <- c(sum(def2)/sum(pob2))
    DifTBM <- TBM1 - TBM2
    
    PesPob1 <- c(pob1)/sum(pob1)
    PesPob2 <- c(pob2)/sum(pob2)
    
    ComPob <- c(tasas2 * (PesPob1 - PesPob2)); ComPob <- sum(ComPob)
    ComTasa <- c(PesPob1 * (tasas1- tasas2)); ComTasa <- sum(ComTasa)
    
    ResultDescomponer <- c(TBM1, TBM2, ComPob, ComTasa)
    
    
    ResultDescomponer
    
}

##### en este ejemplo A = hombres, españa, 2010 y B = mujeres, españa, 2010

pobhom <- c(1266692,  1181426,  1084866,    1150920,    1335161,    1726659,    2081616,    2033138,    1885752,    1714124,    1489824,    1268453,    1159653,    952274,    808087,    729607,    477301,    239005,    70580,    17290)
defhom <- c(1022, 119, 141, 353, 593,    820,    1272,    1873,    3117,    4936,    7128,    9130,    12382,    15398,    20237,    31258,    36588,    31317,    14937,    5500)
pobmuj <- c(1194541,  1116469,    1028245,    1089308,    1284746,    1653324,    1948574,    1912625,    1826140,    1709163,    1523603,    1326613,    1251807,    1063192,    977932,    977266,    749482,    452868,    177749,    52921)
defmuj <- c(888, 80,    86,    152,    221,    316,    552,    899,    1541,    2330,    3161,    3950,    5017,    6692,    10802,    21520,    34286,    43303,    31561,    16569)

TBMEsp2010 <- DesComTBM(defhom, pobhom, defmuj, pobmuj)

TBMEsp2010
# -----------------------------------------------------------------------------------
# unas ideas:
# 1) generalizar los argumentos para que sirve para cualquier tipo de descomposicion
#    de dos 'sumaproductas'
# 2) comentar los pasos dentro de la funcion- 
# 3) dejar aire de respirar en el cuerpo de la funcion
# 4) en vez de un vector, considera devolver una lista con varios elementos
#    e.g. todo lo que podria ser interesante, como vectores de los componentes 
#         de estructura (antes de sumar), que podria sirvir para graficos bonitos?
#    esto supondria la forma: list(TasasBrutas = c(1 = TBM1, 2 = TBM2),
#                                Diferencia = DifTBM,
#                                Componentes = c(estructura = ComPob, tasa = ComTasa),
#                                estructura = ComPobAntesdeSumar,
#                                tasas = ComTasaAntesDeSumar)
# 5) si te parece mucho para devolver (largo y feo), se puede hacer varias cosas:
#   5.a) en vez de 'ResultDescomponer', pon 
#                   invisible(ResultDescomponer) - asi puedes asignar el resultado
#                                                  sin mostrarlo cada vez
#   5.b) si siempre quieres la misma parte, puedes cogerla de la lista en el momento
#        de llamar la funcion, asi:
#        DesComTBM(defhom, pobhom, defmuj, pobmuj)$Componentes
#     o asi:
#        DesComTBM(defhom, pobhom, defmuj, pobmuj)[c("TasasBrutas","Componentes")]
# 6) en la cabezada de la funcion, incluir comentarios describiendo que tienen que ser los argumentos
#    e.g. (vector numerico de ...)
# 7) intenta cambiar la forma para que acepta matrices como argumentos tambien = muchos años de golpe?


# ------------------------------------------------------------------------------
# tabla de vida





