
# Author: triffe
###############################################################################

# Session 2, manipular objectos

# ---------------------------------------------------
# leer unos datos:

# desde un fichero separado con comas:

# al final del sintaxis hay mas maneras de leer datos!

# cambia el destino. file.choose() puede ser util 
NAC <- read.csv("/home/triffe/git/CED-R/CED-R/data/CSVcoma.csv", # cambie esto a donde tienes el fichero
        skip = 8,                  # que salta las primeras 8 lineas- INE ha puesto metadatos alli 
        stringsAsFactors = FALSE,  # quiere decir que si vea algun columna compuesta de caracteres,
                                   # que NO lo convierte en variable categorica. 
        fileEncoding = "latin1")   # para respetar los acentos!

dim(NAC) # 39 filas, 28 columnas.
head(NAC) # la ultima columna es basura..
tail(NAC) # al fondo tambien hay unas filas de basura (mas metadatos del INE)

# --------------------------------------------------
# limpiar los datos

# reducimos los datos a solo las columnas y filas que tengan datos utiles:
# [] se usa para 'indexar', es decir, especificar filas y/o columnas.
# NAC tiene 2 dimensiones, por lo tanto, hay que diferenciar entre filas y
# columnas con una coma:
NAC <- NAC[1:35, 1:27] # filas primero, columnas segundo
                       # seleccionamos solo las primeras 35 filas y 27 columnas (quitando totales tambien)
# para que veas lo que pasa adentro de las []
1:35

head(NAC)
tail(NAC)

# la columna de edades (la primera- [, 1]) tampoco es muy util tal como esta: asignamos edades como integer:
NAC[, 1] <- 15:49 # esto solo funciona si el vector (15:49) que asignas tiene la longitud corecta...

# que pasaria si lo ponemos mal?
NAC[, 1] <- 15:48 # no lo acepta!
NAC[, 1]          # mira, no ha cambiado
# PERO, si por casualidad es divisible:
NAC[, 1] <- 1:5   # no te dice NADA! 
NAC[, 1]          # en otros contextos de programacion, esto es conveniente, 
                  # pero ahora es claramente no deseado. Moral de la historia:
                  # seas conciente de las dimensiones de tus datos...
NAC[, 1] <- 15:49 
# tambien la primera columna tiene un nombre:
colnames(NAC) # "X", porque faltaba un nombre en el csv...
# puedes indexar con nombres tambien:
NAC[, "X"] <- 15:49 
# y hay otras maneras.
NAC$X # e.g. $
# este tipo objecto de llama un 'data.frame' ; es lo mas comun de R
class(NAC)

# parece mucho a una matriz, pero no lo es. Las columnas de un data.frame
# pueden ser de qualquier class de datos (character, numeric, integer, factor, logical..)

# ---------------------------------------------------
# vamos a ver como leerlo de otra manera

# read.table es la funcion mas generica de leer datos; aqui separado con tabuladoras:
# file.choose() # para buscar donde estan los datos.
NAC2 <- read.table("/home/triffe/git/CED-R/CED-R/data/CSVtab.csv",  # donde esta el fichero
                          sep = "\t",                # el fichero esta separado con tabuladoras (" ", ";", ",", etc)
                          skip = 8,                  # salta lsa primeras 8 filas de metadatos
                          header = TRUE,             # si, tiene cabezado
                          stringsAsFactors = FALSE,  # character- no factor
                          nrows = 35,                # para despues de 35 filas leidas
                          fileEncoding = "latin1")   # como tiene acentos
# especifico nrows = 35 porque las filas por debajo son incompletas- nos salva tiempo limpiando en R
# header = TRUE aqui no es por defecto- hay que decirlo
# sep = "\t" especifica tabuladores, existen otros: " ", ";", ",", etc

# para ficheros de amplio fijo, usa: read.fwf(), por ejemplo para microdatos del INE. Haremos ejemplo
# de eso otro dia, no quiero ser pesado ahora
rm(NAC2)
#-----------------------------------------------
# volviendo a objectos:
# Cuales son lo objectos que nos interesan:

# Ya conoces los vectores:
c(3, 6, 7, 8, 3, 3)
# NAC es un data.frame
# realmente un data.frame es un 'list', compuesta por vectores que tengan la misma longitud..
# un list() es el objecto mas flexible de R. 
NAC
# NAC podria ser una matriz:
NACmat <- as.matrix(NAC)
# tambien existen listas:
mi.lista <- list(a = 1:10, 
                 b = list(b1 = 5, b2 = 19, c = 4:45), 
                 mi.matriz = matrix(1:10, ncol = 2))
mi.lista
# listas son absolutamente arbitrarias en cuanto la clase/ longitud/ dimension de sus elementos

# otro ejemplo de list():
# invento (imaginando 5 centuries)

ID1                 <- list() # ahora vacia
# otra manera de componer una lista- elemento por elemento:
# si en el momento no existe el elemento nombrado, se crea al final de la lista
ID1[["ID"]]         <- "62456215_0"  
ID1[["padres"]]     <- c("6245", "6215")
ID1[["hermanos"]]   <- c("62456215_1", "62456215_2")
ID1[["hijos"]]      <- NA
ID1[["esposa"]]     <- "7620530_2"
ID1[["clase"]]      <- 3
ID1[["padreclase"]] <- 3
ID1[["completo"]]   <- TRUE
# se puede meter matrices, data.frames, otras listas; listas dentro de listas-
# e.g. un arbol genealogico de parentesco, o lo que sea.

ID1

# los elementos no tienen que tener la mismas longitud, ni la misma class

# se puede extraer elementos usando los nombres:
(id <- ID1[["ID"]])
# o usando el numero de indice:
(id <- ID1[[1]])
# o usando $:
(id <- ID1$ID)
# si quieres extraer varias cosas, solo hace falta una [ (no [[)
(id_completo <- ID1[c("ID","completo")]) # seleccionando varias cosas, el $ ya no sirve...
# tambien saldra una lista- si lo quieres luego en un vector:
(id_completo_v <- unlist(id_completo))
# pero mira que ha pasado: TRUE, ya no es logico- lo ha cambiado a un character,
# porque vectores solo pueden contener cosas de la misma clase- character consume logico...
# probablemente no lo quieres asi.

# tambien se puede convertir a un data.frame:
(id_complete_df <- as.data.frame(id_completo))
# no es un vector!

class(id_complete_df)

# tambien se puede indexar con vectores logicos

length(ID1) # ten cuidado que el vector que usas tiene la misma longitud
ID1[c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)]
# volvermos a ver ejemplos de vectores logicos usados para indexar...

# --------------------------------------------------------------------------------
# vamos a usar esta estrategia de indexar con vectores para hacer los datos de nacimientos mas utiles:

# para recordar:
head(NAC)
# quiero los datos de varones en una matriz, y los de mujeres en otra matriz, porque si.
colnames(NAC)
# para buscar las columnas con "Varones" en el nombre, usamos grep()
args(grep) # hechamos un vistazo a como especificarlo...
grep(pattern = "Varones", x = colnames(NAC)) # los indices donde aparece "Varones"

NACv <- NAC[, grep(pattern = "Varones", x = colnames(NAC)) ]
NACm <- NAC[, grep(pattern = "Mujeres", x = colnames(NAC)) ]

head(NACv)
# quitamos la ultima columna, solo es un total:
NACv <- NACv[, -13] # igual que NACv[, 1:12]
NACm <- NACm[, 1:12]

# cambiamos los nombre de las columnas a algo entendible:
meses <- c("en", "feb", "marzo", "abr", "mayo", "jun", "jul", "ag", "sept", "oct", "nov", "dic")
colnames(NACv) <- meses 
colnames(NACm) <- meses

# y podemos poner edades como etiquetas de filas:
rownames(NACv) <- 15:49 
rownames(NACm) <- 15:49 

# todavia tenemos un data.frame : convertir a una matriz
class(NACv)
NACv <- as.matrix(NACv)
NACm <- as.matrix(NACm)

# ahora, si nos da igual tener nacimientos por sexo, se puede sumar:
NACt <- NACv + NACm
NACt

# eso funciona cuando las dimensiones son 'conformables'

# de hecho- qualquier operador matematico respeta las dimensiones de vectores y matrices.

# puedes multiplicar cada elemento de la matriz por un numero unico:
NACt * 5
# si haces aritmetica entre vectores y matrices, vectores corresponden con filas!
NACt * rep(.5, nrow(NACt)) # un resultado absurdo: media de chicos y chicas por edad la madre y mes
# etc etc.

# buscamos poblaciones a 1 de enero de 2011 y 2012:
# estos son del padron, posiblemente no los mejores para denominadores
# pero si los mas actuales y estan sin suavizar!
# ya los he puesto bonito en un csv:
pob <- read.csv("/home/triffe/git/CED-R/CED-R/data/Pop.csv")

head(pob)
tail(pob)
dim(pob)
# 

# buscamos los datos de 2011 usando indexacion logica:
pob$Year == 2011 # especificando columna con $
pob$Year == 2011 & pob$Age >= 15 
p2011 <- pob[pob$Year == 2011 & pob$Age >= 15 & pob$Age <= 49, "Female"] # resultado es un vector; edad 15-49; año 2011
p2012 <- pob[pob$Year == 2012 & pob$Age >= 15 & pob$Age <= 49, "Female"]
length(p2011) ; length(p2012) ; nrow(NACt)

# hacemos vectores de la media para aproximar la exposicion:
(Exp <- (p2011 + p2012) / 2) # interpolacion lineal; poblacion 1 julio; llamalo lo que quieres

# olvidamos un momento de meses:
(nacimientos <- rowSums(NACt)) # rowSums() es una funcion buenisima!, tb hay colSums(); rowMeans(); colMeans()..

# tasas especificas por edad:
tasas <- nacimientos / Exp

plot(x = 15:49, y = tasas, type = 'l')

# el ISF (si el padron sobre-estima, esto sera una subestimacion)
sum(tasas)


# -------------------------------------
# mas diversion por debajo!









# aqui se trata de ver en R como usar codigo que igual te parece exotico; no sabes los detalles,
# pero sabes que esta haciendo. Vamos a hacer algo de demografia freaky
# y propondre un ejercicio que te hara pensar en como copiar y pegar codigo efectivamente

# -----------------------------------
# a ver como varien nacimientos por meses?
plot(1:12, colSums(NACt), type = "l")

# esta variacion es real o ruido?

# simulamos de una distribucion poisson
args(rpois)
# enero solo, tiramos los dados 1000 veces
enero.sim <- rpois(n = 1000, lambda = sum(NACt[,1]))
# buscamos los cuantiles .025 y .975

enero95 <- quantile(enero.sim, probs = c(.025, .975))
enero95 ; sum(NACt[,1])
# ahora se podria repetir lo mismo manualmente 12 veces,
# o se puede hacerlo de golpe. Esto es ya mas avanzado,
# por ahora solo replicar y entender que ha hecho. 

meses95 <- sapply(colSums(NACt), function(mes){             # para los con mas curiosidad: sapply() nos hace un bucle
                      sim <- rpois(n = 1000, lambda = mes)  # sobre el vector de nacimientos por mes- es decir elemento
                      quantile(sim, probs = c(.025, .975))  # por elemento. Con cada elemento, hacemos la simulacion
                    }                                       # de arriba (1000 veces), y extraemos intervalos de 95%
                  )
meses95 # nos lo ha puesto en una matriz bonita- el mismo que enero95, pero para cada mes
# no os preocupeis por el sintaxis de arriba, que compiando y pegando se puede lograr mucho!

# el mismo plot, con intervalos simulados de 95% 
plot(1:12, colSums(NACt), type = 'l')
lines(1:12, meses95[1, ], col = "red", lty = 2) # superimponemos una linea
lines(1:12, meses95[2, ], col = "red", lty = 2) # y otra

# o un plot mas bonito:
plot(1:12, colSums(NACt), type = 'l')
# asi me gusta hacer las regiones de (falta de) confianza rellenas..
polygon(c(1:12, 12:1), c(meses95[1, ],rev(meses95[2, ])), col = "#FF000050", border = NA)
                                                              # esto es RGB
# graficamente, afirmo que la variacion entre meses y mucho mas que ruido. Si quieres
# luego pensar en que podria estar detras de eso, habria que subtraer 9 de los meses,
# e igual tomar en cuenta muertos fetales, etc; aqui solo medir


# ahora, si yo invento aqui un indice de la 'magnitud de la estacionalidad' (no en el sentido de series temporales, sino mas en el sentido Vivaldi!),
# a ver si me puedes decir si son los chicos o las chicas mas estacionales en 2011?
TOT <- sum(mnths)
(sum(abs(mnths - (TOT / length(mnths)))) / 2) / TOT

# un indicador que varie entre 0 y 1: 
# 0- perfectamente igual en cada mes (semana,dia) ; 
# 1- todos eventos concentrados en una unidad de tiempo (1 es solo posible cuando el intervalo -> 0)
# (el indicador es algo parecido a los indicadores de age-heaping por preferecia de digitos)
Estac <- function(eventos){
    tot <- sum(eventos) # eventos totales
    sum(abs(eventos - tot / length(eventos)) / 2) / tot
}

# proporcion de nacimientos que tendrian que pasar en otro mes para no detectar
# nada de 'estacionalidad' en nuestro sentido sencillo
Estac(colSums(NACt))
# es significativo?
Estac.sims <- sapply(colSums(NACt), function(mes){   # para los nacimientos de cada mes, uno por uno,
            rpois(n = 1000, lambda = mes)            # simulamos la cantidad 1000 veces, tirando los
        })                                           # dados de una distribucion poisson
dim(Estac.sims) # 1000 filas, 12 columnas
# columnas son meses
# filas son replicaciones de la simulacion

# calculamos nuestro indicador de estacionalidad para cada fila:
Estac.sim.Ind <- apply(Estac.sims, 1, Estac) # aqui evitamos otro bucle!: nos applica una funcion (la nuestra) en cada fila
hist(Estac.sim.Ind) # mira la histograma
Estac(colSums(NACt)) ; quantile(Estac.sim.Ind, probs = c(.025, .975))
# pues si, parece una estimacion algo estable.

# retos, a ver si me podeis decir, tras copiando y pegando creativamente
# 1) son chicos o chicas en 2011 los que tengan el valor mas alto de 'Estac()'?
# 2) parece ser una diferencia creible?
# 3) la edad de la madre tiene un efecto?- a ver si puedes acercar a una respuesta sin uso de estadistica inferencial.


