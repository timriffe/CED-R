
# Author: triffe
###############################################################################

# ------------------------------------------------------------------
# de Amand:
# ------------------------------------------------------------------
##### descomponer diferencias entre TBM

DesComTBM <- function(def1, pob1, def2, pob2){

    # def1, un vector numerico de defunciones para poblacion 1
    # pob1 
  
    # calcular tasa brutas
    TBM1 <- c(sum(def1)/sum(pob1))
    TBM2 <- c(sum(def2)/sum(pob2))
    
    # la diferencia que queremos descomponer
    DifTBM <- TBM1 - TBM2
    
    # calcular tasas especificas
    tasas1 <- c(def1/pob1)
    tasas2 <- c(def2/pob2) 
  
    PesPob1 <- c(pob1)/sum(pob1)
    PesPob2 <- c(pob2)/sum(pob2)
    
    ComPobvec <- c(tasas2 * (PesPob1 - PesPob2))
    
    ComPob <- sum(ComPobvec)
    
    ComTasavec <- c(PesPob1 * (tasas1- tasas2))
    ComTasa <- sum(ComTasavec)
      
    ResultDescomponer <- list(tasasbrutas = c("1"= TBM1,"2"=TBM2),
                              diferencia = DifTBM,
                              componentes = c(Pob = ComPob, Tasa = ComTasa),
                              estructura = ComPobvec,
                              tasas = ComTasavec)
    
    
    #ResultDescomponer <- c(TBM1, TBM2, ComPob, ComTasa)
    
    invisible(ResultDescomponer)
}

# ##### en este ejemplo A = hombres, españa, 2010 y B = mujeres, españa, 2010
# 
# pobhom <- c(1266692,  1181426,  1084866,    1150920,    1335161,    1726659,    2081616,    2033138,    1885752,    1714124,    1489824,    1268453,    1159653,    952274,    808087,    729607,    477301,    239005,    70580,    17290)
# defhom <- c(1022, 119, 141, 353, 593,    820,    1272,    1873,    3117,    4936,    7128,    9130,    12382,    15398,    20237,    31258,    36588,    31317,    14937,    5500)
# pobmuj <- c(1194541,  1116469,    1028245,    1089308,    1284746,    1653324,    1948574,    1912625,    1826140,    1709163,    1523603,    1326613,    1251807,    1063192,    977932,    977266,    749482,    452868,    177749,    52921)
# defmuj <- c(888, 80,    86,    152,    221,    316,    552,    899,    1541,    2330,    3161,    3950,    5017,    6692,    10802,    21520,    34286,    43303,    31561,    16569)
# 
# TBMEsp2010 <- DesComTBM(defhom, pobhom, defmuj, pobmuj)
# TBMEsp2010[1] - TBMEsp2010[2]
# sum(TBMEsp2010[3:4])
# DesComTBM(defhom, pobhom, defmuj, pobmuj)$componentes
# DesComTBM(defhom, pobhom, defmuj, pobmuj)[["componentes"]]
# DesComTBM(defhom, pobhom, defmuj, pobmuj)[c("tasasbrutas", "componentes")]
# 
# 
# TBMEsp2010
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

def <- read.csv("/home/triffe/git/CED-R/CED-R/data/def2011.csv")
pob <- read.csv("/home/triffe/git/CED-R/CED-R/data/Pop2011.csv")


Dx <- def[,2]
Px <- pob[,"Hombres"] 
Dx[length(Px)] <- sum(Dx[length(Px):length(Dx)])
Dx <- Dx[1:length(Px)]
length(Dx)
length(Px)


TabladeVida <- function(Dx, Px, sexo = "hombres", l0 = 100000){
  
  if (length(Dx) != length(Px)){
    stop("Dx y Px tienen que tener la misma longitud")
  }
  
  if (!(is.numeric(Dx) | is.integer(Dx)){
    stop("argumento Dx no era numerico!")
  }
  
  if (sexo == "varones" | sexo == "homes" | sexo == "v" | sexo == "h"){
    sexo <- "hombres"
  }
  
  # Dx: un vector numerico de defunciones por edades
  # Px ...
  
  N <- length(Dx)
  edades <- 0:(N-1)
  
  # tasas centrales
  Mx <- Dx / Px
  
  # edad media al morir dentro del intervalo
  ax <- rep(.5, N)
  
  # edad 0 es diferente
  ax[1] <- ifelse( sexo == "hombres",{.045 + 2.684*Mx[1]}, # hombres
                                     {.053+2.8*Mx[1]}      # mujeres
                 )
  
  # probabilidad condicional de morir
  qx <- Mx / (1 + (1 - ax) * Mx)
  
  if (max(qx) > 1){
    cat("cuidado! qx ha tenido un valor > 1\n")
    qx[qx > 1] <- 1
  }
  qx[N] <- 1
  
  # de sobrevivir
  px <- 1 - qx
  
  # curva de supervivencia
  lx <- c( l0, l0 * cumprod(px[1:(N - 1)]))
  
  # defunciones (dentro de la tabla de vida)
  dx <- lx * qx
  
  # Lx = exposicion dentro de la tabla de vida
  Lx <- lx[2:N] +  ax[1:(N-1)] * dx[1:(N-1)]
  Lx[N] <- lx[N] / Mx[N] # cerrar Lx
  
#   Lx2 <- lx - (1-ax) * dx
#   Lx2[N] <- lx[N] / Mx[N]
  
  # Tx: años que queden para vivir de los que han sobrevivido
  # hasta la edad x
  Tx <- rev(cumsum(rev(Lx)))
  
  # esperanza de vida restante
  ex <- Tx / lx
 
  tablaResultado <- data.frame(edades = edades,
                               Defunciones = Dx,
                               Exposicion = Px,
                               Mx = Mx,
                               ax = ax,
                               qx = qx,
                               lx = lx,
                               Lx = Lx,
                               ex = ex)
  invisible(tablaResultado)
}

TV <- TabladeVida(Dx, Px)
head(TV)

# -------------------------------------------------------------


