
# Author: triffe
###############################################################################
# Graficos!

# ejecute el codigo desde aqui hasta la linea 100 antes de clase, para tener los datos listo.
# explicare todo en clase- menos mal si lees esto primero.
# -----------------------------------------------------------------------------
# pero claro, para hacer graficos, necesitamos datos.
# -----------------------------------------------------------------------------
# esta vez leemos desde un fichero ASCII de ancho fijo.

# datos de ejemplo: EVR, 2010 con todos municipios

# ----------------------------------------------------------------------------
# manipulamos el fichero zip diractamente desde R- asi no hace falta tener una 
# carpeta de ficheros grandes descomprimidos.
# podrias descomprimir manualmente tambien, pero hacerlo desde R te puede ayudar
# en automatizar cosas [imagina que tengas muchos años, cada uno en un fichero distinto,
# pero con nombres regulares]

# 1) cambiar esta direccion a donde tienes 'datos_vr10.zip'
# file.choose() # para ayudar buscar donde tienes el fichero 
direccion.de.mi.fichero.zip <- "/home/triffe/workspace/Migs/DATA/ORIG_ZIP/datos_vr10.zip"
# hacemos una carpeta temporario- acabaremos eliminandolo luego.
(zip.dir <- tempfile()) # lo pongo todo en parentesis para que tambien veas el resultado
dir.create(zip.dir)     # nos hace una carpeta en el ordenador

# extraer el fichero a la carpeta que acabamos de crear: unzip()
unzip(zipfile = direccion.de.mi.fichero.zip , exdir = zip.dir)

# el nombre del fichero [si hay mas ficheros, te da un vector character con todos los nombres]
(unzipped.file <- list.files(zip.dir))
# la direccion del fichero con que trabajaremos:
(unzipped.path <- file.path(zip.dir, unzipped.file)) # esto lo podrias hacer a mano tb
                                                # pongo file.path() porque sea una funcion lista
                                                # y usa el separador corecto segun tu tipo de sistema
                                                # e.g. windows, mac, linux

# preparemos unos vectores para pasar como argumentos cuando leemos los datos:
# estas etiquetas valen para los años 2005-2010- nosotros solo vamos a usar los del 2010
# las he buscado en el fichero de metadatos. Cada fichero de microdatos del INE
# tiene uno o mas ficheros auxiliares con la decripcion y posicion de los variables en el
# fichero de ancho fijo.


varnames <- c("SEXO", "PROV1", "MUNI1", "DIANACI", "MESNACI", "ANONACI", "CNAC", "PROV2", 
              "MUNI2", "DIARECEP", "MESRECEP", "ANORECEP","PROV3","MUNI3","TAMUALTA","TAMUBAJA")
# no hacen falta posiciones, solo anchos
widths   <- c(1, 2, 3, 2, 2, 4, 3, 2, 3, 2, 2, 4, 2, 3, 1, 1)
# cc pongo, pero no es necesario- le digo por adelantado porque 
#                   1) lo lee mas rapido asi y
#                   2) no habran sorpresas
cc <- c("integer", "character", "character", "integer", "integer", "integer", "character", "character",
             "character", "integer", "integer", "integer", "character", "character", "integer", "integer")

# ----------------------------------------------------------------------------
# envia el siguiente bloque de codigo a R, despues lee todo esto!
# ----------------------------------------------------------------------------
# read.fwf() es la funcion que nos hace el trabajo. Si tu fichero tiene mas de 1e6 filas, funcionara igual,
# pero hay mejores estrategias. Ya con este fichero dura un buen rato. Porque? 
# Porque R lo quiere tener completamente en memoria activa. Despues sera mas agil trabajarlo en vivo. 
# Si el fichero que leas es (muy) grande (esto no cuenta como muy grande) y no sabes si tienes memoria suficiente:
# abre tu gestor de tareas y mira el uso de memoria. Si parece que llegara al techo- mata el proceso de R desde alli.
# despues ves a preguntar el Juan Galeano como hacerlo usando sqldf() en el package 'sqldf'
EVR <- read.fwf(file = unzipped.path,       # donde esta el fichero descomprimido
                widths = widths,            # widths lo comunica donde cortar variables nuevos
                header = FALSE,             # el fichero no tiene cabezado
                col.names = varnames,       # si queremos dar etiquetas a las columnas (no obligatorio)
                stringsAsFactors = FALSE,   # para dejar caracteres como caracteres y no variables categoricos
                colClasses = cc             # las clases de columnas- si no dices que MUNI1 sea caracter- 
               )                            #                         la funcion va a pensar que sea integer
                                            #                         cambiara '001' a 1. luego no te sirve como un ID..
# mas informacion aqui: ?read.fwf
load("/home/triffe/workspace/Migs/DATA/Rdata/EVR2010.Rdata")
# tamaño aproximado del fichero descomprimido en tu sistema:
file.info(unzipped.path)$size / 1048576 # bueno, depende del sistema
# cuanta memoria ocupa cuando esta dentro de R:
print(object.size(EVR), units = "Mb") # vez como se ha puesto mas grande en R que fuera
# si fuera una matriz numerica estaria considerablemente mas pequeño. queremos data.frame
# porque tenemos columnas de 2 clases distintas.

# cambiar el fichero a un formato util cuesta trabajo. Si lo vas a usar en esta forma
# mas de una vez, mejor guardarlo como un binario nativo de R...
save(EVR, file = "/home/triffe/workspace/Migs/DATA/Rdata/EVR2010.Rdata")
# luego lo cargas usando: (cambiando la direccion
# load("/home/triffe/workspace/Migs/DATA/Rdata/EVR2010.Rdata") # y aparecera un objecto "EVR" en tu espacio de trabajo
# truco: si quieres asignarlo directamente a otro nombre, haz:
# EVR.tu.nombre <- local(get(load("EVR2010.Rdata")))

# una vez hecho lo de arriba, eliminamos el fichero externo descomprimido
file.remove(unzipped.path)
file.remove(zip.dir)

# ya esta- el resto lo haremos en la session- continua si quieres,
# pero hare bastantes cambios entre ahora y la session.



ls()
# ---------------------------------------------------------------
# ahora vamos a trabajar con los datos del EVR 2010
dim(EVR) # 2,5 milliones de casos! 
prod(dim(EVR)) # numero de celdas
head(EVR)
# tenemos:
# PROV1, MUNI1 : luger nacimiento
# PROV2, MUNI2 : lugar origin
# PROV3, MUNI3 : lugar destino

# tambien hay:
# nacionalidad (CNAC)
# fecha exacta de nacimiento
# fechas exacta de la registacion del movimiento
# alguna categorizacion del tamaño del municipio.

# 1) hacemos 2 columnas nuevas para represantar fechas:
#    porque? porque asi nos saldra mas facil calcular una edad exacta 
#    asi... se podria hacer triangulitos Lexis para una Spanish Migration Database, jeje

# queremos, e.g. as.Date("2010-1-4") + 10

# se puede concatenar usando paste() para poner las fechas en el formato deseado:
# por ejemplo:

(unas.fechas.inventadas.char <- paste(rep("2010", 5), # año 2010
        sample(1:12, size = 5, replace = TRUE),       # unos meses aleatorios
        sample(1:29, size = 5, replace = TRUE),       # unos dias aleatorios
        sep = "-"))                                   # separado con '-'
(unas.fechas.inventadas.Date <- as.Date(unas.fechas.inventadas.char))
# parece character, pero no lo es: te muestra la fechas de manera que lo entiendes,
# pero realmente esta representado con un numero (en dias)
unas.fechas.inventadas.Date - 10
as.integer(unas.fechas.inventadas.Date) # si?
# te hacer preguntar- que se considera fecha 0?
unas.fechas.inventadas.Date - as.integer(unas.fechas.inventadas.Date) # tiempo empieza el 1 de enero, 1970!
# ?as.Date # aqui lo aprendes todo
# nos da igual- fechas antes seran negativos, y los calculos funcionan igual.

# ahora, todo de golpe (tarda un poco en hacer...)
EVR$FechaN <- as.Date(paste(EVR$ANONACI, EVR$MESNACI, EVR$DIANACI, sep = "-"))
EVR$FechaM <- as.Date(paste(EVR$ANORECEP, EVR$MESRECEP, EVR$DIARECEP, sep = "-"))

head(EVR)


hist(as.integer(EVR$FechaM - EVR$FechaN), main = "edad en dias en el EVR, 2010")
# lo guardamos asi, dividiendo por 365.25, una aproximacion aceptable
EVR$EdadExacta <- as.numeric((EVR$FechaM - EVR$FechaN) / 365.25)
# en años cumplidos floor() va al integer abajo /  ceiling() al integer arriba / round() va al decimal que tu digas
EVR$Edad       <- floor(EVR$EdadExacta)
EVR$Edad[1:10]
# olvidando del espacio, aqui las frequencias en edades sencillas:
table(EVR$Edad)
# mejor, por edad y sexo:

(SexoEdad <- table(EVR$Edad, EVR$SEXO))

head(SexoEdad)

# una piramide de migraciones?
# he hecho un package para piramides, Pyramid()
# lo puedes installar tal cual desde aqui https://sites.google.com/site/timriffepersonal/r-code/packagedownloads
#, o activar el codigo directo del internet (mas rapido):

# si no estas en windows, lo siguiente funciona:
# install.packages("devtools") # tienes que primero conectar al internet, si estas en la Aula Informatica...
#library(devtools) # para cargar la funcion source_url()
#source_url("http://raw.github.com/timriffe/Pyramid/master/Pyramid/R/Pyramid.R")

# solucion para windows
source(pipe(paste("wget -O -", "http://raw.github.com/timriffe/Pyramid/master/Pyramid/R/Pyramid.R")))

# si estas en windows, mejor copiar y pegar el fichero dentro de un fichero que se llama Pyramid.R
# y usar source("direccion/del/fichero.R") # para cargarlo
# file.choose()
# source("/home/triffe/git/CED-R/CED-R/Pyramid.R")
# ahora tienes una funcion que se llama Pyramid()
Pyramid(males = SexoEdad[,1], females = SexoEdad[, 2])
args(Pyramid)
# hay muchos argumentos posibles
args(Pyramid)




# por defecto te pone los ejes en terminos de proporcion.
# para ilustrar unos de los argumentos:
Pyramid(males = SexoEdad[,1], females = SexoEdad[, 2], 
        coh.axis = TRUE, 
        year = 2010,
        fill.males = gray(.6),
        fill.females = gray(.4),
        grid.bg = gray(.9),    # fondo gris
        grid.lty = 1,          # lineas de malla solidas
        grid.col = "white",    # malla blanca
        grid.lwd = .5,         # con lineas delgadas
        main = "Spain moves 2010, by sex and age of mover\ndata from EVR www.ines.es") # titulo (\n forma una linea nueva)
# tienes bastante control del grafico. Si quieres control total, como no puedo pensar en todo,
# tendras que hacer el grafico desde los elementos graficos mas basicos. Mira el codigo dentro de Pyramid()
# y modifiquelo a lo que necesitas

# -------------------------------------------------------------------------------
# lo mismo, guardando como un pdf:
pdf("PiramideEVR2010.pdf") # primero define un pdf con un nombre (no olvides del '.pdf')
# ahora haz el grafico mas complejo que quieras. ira guardandolo dentro del pdf 
# normalmente no veas nada.
Pyramid(males = SexoEdad[,1], females = SexoEdad[, 2], 
        coh.axis = TRUE, year = 2010, fill.males = gray(.6), fill.females = gray(.4),
        grid.bg = gray(.9), grid.lty = 1, grid.col = "white",
        grid.lwd = .5, main = "Spain moves 2010, by sex and age of mover\ndata from EVR www.ines.es")
dev.off() # para acabar la grabacion del grafico, cerrar el pdf

# donde lo ha guardado?
getwd() # aqui

# o en un .emf para Word o powerpoint
# install.packages("devEMF")
library(devEMF)
emf(file = "PiramideEVR2010.emf")
Pyramid(males = SexoEdad[,1], females = SexoEdad[, 2], 
        coh.axis = TRUE, year = 2010, fill.males = gray(.6), fill.females = gray(.4),
        grid.bg = gray(.9), grid.lty = 1, grid.col = "white",
        grid.lwd = .5, main = "Spain moves 2010, by sex and age of mover\ndata from EVR www.ines.es")
dev.off()

getwd()
# luego, dentro de Word lo insertes como un fichero de imagen

# tambien existen otros formatos utiles: 
# raster : bmp(), png(), tiff() [NUNCA guarda graficos estadisticos como jpeg()!]
# en estos graficos especifiques las dimensiones en pixels (por defecto 400x400), pero
# puedes cambiar la resolucion. Mas resolucion  = fichero mas grande. menos resolucion = grafico chungo
# *mejor preferir formato vector

# otros vector: pdf(), svg()


# -----------------------------------------------------------------------------------------------
# hacer graficos en R siempre es un proceso iterativo-
# vas añadiendo elementos, probando, quitando, regenerando
# hasta que te guste el resultado. El tema de graficos en R
# es muy grande, y puede resultar util leer mas o mirar paginas web.

# hay un libro "R Graphics" que tiene una pagina web con el codigo de sus
# graficos: http://www.stat.auckland.ac.nz/~paul/RGraphics/rgraphics.html

# o para mirar muchos ejemplos de codigo de lattice:
# el libro de Springer 'Lattice: Multivariate Data Visualization with R'
# http://lmdvr.r-forge.r-project.org/figures/figures.html

# y ggplot2: 'ggplot2: elegant graphics for data analysis'
# (Daniel tiene el libro )
# http://ggplot2.org/book/

# esto es mi favorito:
# http://gallery.r-enthusiasts.com/

# mas para demografos:
# he puesto tutorios de como hacer superficies lexis en mi blog usando
# tanto en Lattice, como en ggplot2 como en 'base'

# Pyramid() esta escrito en base- se podria replicar en los demas sistemas igualmente.
# hoy solo vamos a experimentar con las funciones y argumentos mas basicos de 'base',
# pero intentaremos simplificar tareas que en Excel y Word estarian dificiles o inpensables.

# -----------------------------------------------------------------------------
# 1) poner muchos graficos en una malla:

# a ver que grafico haremos? algo por provincies tiene que ser
# provincia destino porque si:
SexoEdadProv <- table(EVR$Edad, EVR$SEXO, EVR$PROV3)
dim(SexoEdadProv) # es 3 dimensional!
class(SexoEdadProv)
dimnames(SexoEdadProv) # edades 0-109 ; sexo ; 52 provincias + extranjero (66)

# un 'array' es una matriz pero con mas dimensiones. eso quiere decir que es un vector con muchas dimensiones.
# aqui solo 3
SexoEdadProv <- as.array(SexoEdadProv)
# se podria cambiar la forma, pero podemos usar esto tal cual tambien.

# esta funcion la puedes mirar en tu propio tiempo para ver que hace y como funciona
# ahora solo seleccionar
PiramideSencillo <- function(SexoEdad, xlim = c(-.03, .03), prov){
    # convertir en grupos quinquenales:
    edades  <- as.integer(rownames(SexoEdad))
    edades5 <- edades - edades %% 5
    widths  <- as.vector(table(edades5))
    # hacemos un bucle interno sobre las columnas de SexoEdad (2 siginifica columnas)
    SexoEdad5 <- apply(SexoEdad, 2, function(x){
                           # ahora x es una columna (hombres o mujeres)
                           # sumamos los grupos quinquenales, agrupando segun el factor 'edades5'
                           tapply(x, edades5, sum)
                         }
                       )
    # cambiar a proporciones (estandardizadas a edades sencillas):
    SexoEdad5 <- SexoEdad5 / (sum(SexoEdad5) * 5)
    # los hombre can hacia la izquierda- haz negativo:
    SexoEdad5[, 1] <- -SexoEdad5[, 1]
    # ahora hacer el grafico:
    barplot(SexoEdad5[, 1], 
            width = widths,     # ancho de las barras
            horiz = TRUE,       # orientacion horizontal
            space = 0,          # pegados
            border = FALSE,     # barras sin franjas
            xlim = xlim,        # los limites del eje x (negativo lado hombres- aqui lo pasamos como un argumento desde arriba)
            ylim = c(0, 90),    # limites y, fijadas
            ylab = "",          # no queremos etiquetas
            xlab = "",
            axes = FALSE,       # tampoco queremos ejes
            axisnames = FALSE,  # nada nada
            names.arg = NA,     # nada nada
            xpd = TRUE)         # permiso de dibujar fuera del area si sea necesario
    par(new = TRUE) # superimponer el otro lado (mujeres)
    barplot(SexoEdad5[, 2], width = widths, horiz = TRUE, space = 0, xlim = xlim, ylim = c(0, 90), axes = FALSE, 
            xlab = "", ylab = "", axisnames = FALSE, border = FALSE, xpd = TRUE, names.arg = NA)
    text(x = -.02, y = 0, labels = prov, xpd = TRUE) # codigo o nombre de provincia
    segments(0, 0, 0, 90, col = "white", lwd = .2)   # una linea blanca, delgada entre hombres y mujeres
}

(cod.prov <- dimnames(SexoEdadProv)[[3]])


pdf("pyramidtable.pdf")
# guarda parametros globales de plotting
op <- par(no.readonly = TRUE)
# con un poco margen abajo y arriba, pero nada en los lados
par(mfrow = c(8, 7),         # queremos 8 filas y 7 columnas de graficos,
    mar = c(.2, 0, .1, 0),  # margenes para cada grafiquito: c(abajo, izquierda, arriba, derecha)
    oma = c(1,1,3,1),        # margenes externales del conunto de graficos
    xpd = TRUE)              # le damos permiso de dibjuar en los margenes        
# uso del bucle 'for':


for (i in 1:53){  # '{' es necesario - se cierre al final
      
  # [todas columnas, todas filas, 'planta i'] (sale una matriz)
    PiramideSencillo(SexoEdadProv[, , i], 
                     xlim = c(-.025, .025), 
                     prov = cod.prov[i]) # xlim, prov, son argumentos de nuestra funcion arrib
    }
# haremos mas bucles en otros momentos. En R, bucles normalmente no son necesarios,
# pero existen, y pueden a veces ser utiles.
PiramideSencillo(SexoEdad, xlim = c(-.025, .025), prov = "total")


segments(0, 0, .02, 0, lwd = .5)
segments(.02, 0, .02, 5, lwd = .5); text(.02, 3, "2%", cex = .7, pos = 3)
segments(.01, 0, .01, 5, lwd = .5); text(.01, 3, "1%", cex = .7, pos = 3)


par(op) # unificar las coordinadadas
dev.off()
# claramente, se podria estar un buen rato deseñando este grafico y valdria la pena
# como el bucle nos salva mucho tiempo replicando, alineando, etc.
# hay otras maneras de sacar un resultado parecido, vea lattice

# -----------------------------------------------------------------------------------
# a ver un scatterplot mas complejo- un proceso muy iterativo- 
# sigues hasta que te gusta el producto

# edad media del migrante, varones x mujeres, con diametro del punto como funcion
# de migrantes totales?:

edades.5 <- .5:109.5 # mitad de intervalo
(em <- matrix(nrow = 53, ncol = 2, dimnames = list(1:53,c("varones","mujeres")))) # matriz para rellenar
# usando bucle:
i <- 1

for (i in 1:53){
    em[i, ] <- colSums(edades.5 * SexoEdadProv[, , i]) / 
      colSums(SexoEdadProv[, , i])
}
# ------------------------------------------
# primera vista:
plot(em[, 1], em[, 2])
# ------------------------------------------
# ajustando rango de los ejes:
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40))
abline(0,1) # linea de igualdad

a <- rnorm(10000)
b <- rnorm(10000)
plot(a,b, pch = 19, col = "#FF000010")


# ------------------------------------------
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            # punto solido
        col = "#00000040"    # "#RRVVAAOO" (rojo, verde, azul, opacidad) = #00000040 = negro con 40% opacidad
)    
abline(0,1, col = gray(.5)) # linea de igualdad

# ------------------------------------------
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            # punto solido
        col = "#0055FF80"    # azul con algo de verde, mas opacidad
)    
abline(0,1, col = gray(.5)) 

# ------------------------------------------
# variamos diametro del punto segun numero de migraciones:
nr.migs <- colSums(colSums(SexoEdadProv))

nr.migs <- vector(length = 53)
for (j in 1:53){
  nr.migs[j] <- sum(SexoEdadProv[, , j])
}
nr.migs <- nr.migs[-length(nr.migs)]
# quitamos nr 66:
#nr.migs <- nr.migs[1:(length(nr.migs) - 1)]
em      <- em[1:(nrow(em) - 1), ]

plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,             # punto solido
        col = "#0055FF80",    # azul con algo de verde, mas opacidad
        cex = nr.migs / sum(nr.migs) * 100 # mal!
)  

# absurdo- mejor cambar la area del circulo, no el diametro

# ------------------------------------------
#area = pi*r^2
sqrt(nr.migs / pi) # sclamos el radio
diametro.puntos <- sqrt(nr.migs / pi) / 25
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,             # punto solido
        col = "#0055FF60",    # azul con algo de verde, mas opacidad
        cex = diametro.puntos # mejor
)  
# ------------------------------------------
# poner etiquetas, titulo
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            
        col = "#0055FF60",   
        cex = diametro.puntos,
        main = "cruzando variables graficamente", # titulo
        xlab = "edad media emigrantes varones",
        ylab = "edad media emigrantes mujeres"
)  
# ------------------------------------------
# variar color segun otro variable continuo?

SexoProv <- colSums(SexoEdadProv)
rel.sexo <- SexoProv[1, ] / SexoProv[2, ]

plot(sort(rel.sexo )) # como varia la relacion de sexo de emigrantes..
plot(log(sort(rel.sexo ))) # como hay >1 y >1, hacemos log
rel.sexo <- log(rel.sexo )

# quiero variar el color 'continuamente'. Tenemos que definir una 'rampa de colores'
# en R puedes hacer el color que quieras, mientres sabes la combinacion HEX. Tambien existen
# packages para hacerte la vida mas facil:
# install.packages("RColorBrewer")
library(RColorBrewer) # Cynthia Brewer es como la reina de color en el mundo de cartografia y graficos estadisticos
                      # www.colorbrewer.org ; tambien nos ha hecho un package
# a ver cuales paletas nos da:
display.brewer.all()
# 'spectral' me parece adecuado.
# es una paleta 'divergente' es decir, con 2 polos-
(spec.colores <- brewer.pal(11,"Spectral"))
# ahora queremos poder interpolar entre estos colores:
library(grDevices) # viene con 'base'
# colorRampPalette() nos hace una funcion de paleta- tu dices cuantos colores
spec.col.ramp <- colorRampPalette(spec.colores, space = "Lab")
# spec.col.ramp() es una funcion
spec.col.ramp(100) # asi podemos aproximar una rampa continua de colores:
# ahora tenemos que asignar colores segun el valor de relacion de sexo de emigrantes.
# 

# podriamos decir que el centro sea 1 (0 despues del logoritmo),
# pero mas adecuado seria la relacion de sexo global de la muestra, no?
RSglobal <- log(sum(SexoProv[1, ]) / sum(SexoProv[2, ])) 
rel.sexo <- log(rel.sexo)
# ahora podemos definir unos intervalos (breaks) inteligentes:
plot(sort(rel.sexo )) 
abline(h = RSglobal)
# para poder tener RSglobal en el medio, mas facil tratarlo como 0
rel.sexo.resc <- rel.sexo - RSglobal
(breaks <- seq(-.36,.36,by = .005))
# esto nos divide el vector de valores en un factor, con etiquetas que son colores:
col.vec <- cut(rel.sexo.resc, 
               breaks = breaks,                            # intervales
               labels = spec.col.ramp(length(breaks) - 1)) # las etiquetas (1 menos k el numero de intervalos)


class(col.vec) # convertimos a character y tenemos un vector de colores que corresponden con nuestros valores :-)
col.vec <- as.character(col.vec)
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            
        col = col.vec,   # cada color corresponde con un punto, pero son solidos....
        cex = diametro.puntos,
        main = "cruzando variables graficamente",
        xlab = "edad media emigrantes varones",
        ylab = "edad media emigrantes mujeres"
)  
# -------------------------------------
col.vec.op <- paste0(col.vec, 70) # 50% opacidad
plot(em[, 1], em[, 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            
        col = col.vec.op,   # cada color corresponde con un punto, pero son solidos....
        cex = diametro.puntos,
        main = "cruzando variables graficamente",
        xlab = "edad media emigrantes varones",
        ylab = "edad media emigrantes mujeres"
)  

text(em[,1],em[,2],labels = 1:52, cex = .5)

# -------------------------------------
# todavia, nos gustaria que primero dibuje los circulos mas grandes,
# y acabar con los circulos mas pequeños
plot.order <- rev(order(diametro.puntos))
plot(em[plot.order , 1], em[plot.order , 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            
        col = col.vec.op[plot.order ],   # cada color corresponde con un punto, pero son solidos....
        cex = diametro.puntos[plot.order ],
        main = "cruzando variables graficamente",
        xlab = "edad media emigrantes varones",
        ylab = "edad media emigrantes mujeres"
)
# -------------------------------------
# alguna leyenda:
plot(em[plot.order , 1], em[plot.order , 2],
        xlim = c(30,40),
        ylim = c(30,40),
        pch = 19,            
        col = col.vec.op[plot.order ],   # cada color corresponde con un punto, pero son solidos....
        cex = diametro.puntos[plot.order ],
        main = "cruzando variables graficamente",
        xlab = "edad media emigrantes varones",
        ylab = "edad media emigrantes mujeres"
)
legend("bottomright",col = rev(paste0(spec.col.ramp(7))), pch = 19, pt.cex = 2, 
        legend = c("muy masculino","masculino","algo masculino","media","algo femenino","femenino","muy femenino"))
# igual este grafico no es no mas interesante-
# lo puedes replicar para municipios?

# -----------------------------------------------------------------------------------
# a ver que mas se puede hacer...
head(EVR)
# quiero hacer algo para los municipios.
length(unique(EVR$MUNI1))
# falta un identificador unico para los municipios, que seria la concatenacion
# de provincia y municipio.
EVR$PROVMUN3 <- paste0(EVR$PROV3, EVR$MUNI3)
length(unique(EVR$PROVMUN3))# esto es mas razonable aparentamente no cada
                            # municipio ha sido un origin- toma en cuenta
                            # que esto incluye registraciones de movimientos
                            # que empiezan o acaban en el extranjero.

# ahora que tenemos un identificador unico para muncipios de origen,
# se puede usarlo para agrupar. Por ejemplo- para calcular la edad media 
# de migrantes desde cada municipio (que haya producido un emigrante)
# no hace falta una table de vida- como tenemos las edades exactas de cada
# migrante:

# usando tapply() es un calculo super directo:
EdadMedEmig <- tapply(EVR$EdadExacta, EVR$PROVMUN3, mean)
hist(EdadMedEmig)            # histograma
plot(density(EdadMedEmig))   # densidad suavizada


# ahora otra vex pero solo con migraciones con origen en España:
EVRD <- EVR[EVR$PROV3 != "66", ]
EdadMedEmig <- tapply(EVRD$EdadExacta, EVRD$PROVMUN3, mean)
plot(density(EdadMedEmig))  
# claro, esto incluye paises de origen- ignoramos por ahora.
# ahora, miramos lo mismo, pero en formato de boxplot segun tamaño 
# del municipio de origen:
PMIDs <- names(EdadMedEmig)


# ahora, buscamos el variable de tamaño para cada identificador en PMIDs,
# y los ponemos en un vector en el mismo orden.
# primero lo hacemos usando un bucle:

# predefinir vector, que luego rellenamos:
TamOr <- vector(length = length(PMIDs))
for (i in 1:length(PMIDs)){                                  # explico: EVR$TAMUBAJA es un vector con 2.5 milliones de elementos
    TamOr[i] <- EVRD$TAMUBAJA[EVRD$PROVMUN3 == PMIDs[i]][1]  # [EVR$PROVMUN2 == PMIDs[i]] nos extrae solo los TAMUBAJA donde el
}                                                            # identificador es igual al que buscamos. todavia pueden haber mucho
table(TamOr)                                                 # como todos tendran el mismo valor, solo cogemos el primero
# es lento porque en cada paso de los 800 tiene que trabajar con 2 vectores de 2.1 milliones de casos (EVRD un poco mas pequeño),
# y tambien porque bucles no son muy rapidos en R.

# el siguiente hace el mismo, pero usando la vectorizacion inherente en R:

# 1) concatenar un vector de todas las combinaciones posibles:
TamO.PM2.combo <- unique(paste(EVRD$TAMUBAJA, EVRD$PROVMUN3, sep = "-"))
TamO.PM2.combo[1:10]   # tamaño-ID
# ahora sabemos cual va con cual
# 2) rompemos el vector sobre el '-'
strsplit(TamO.PM2.combo, split = "-")  # solo para ver que te devuelve- una lista
# es demasiado largo por los origenes internacionales.

# deshacemos la lista, y lo ponemos en una matriz con 2 columnas:
TamO.PM2.combo <- matrix(unlist(strsplit(TamO.PM2.combo, split = "-") ), ncol = 2, byrow=TRUE)
TamO.PM2.combo[,2] == PMIDs # no estan en el mismo orden...
rownames(TamO.PM2.combo) <- TamO.PM2.combo[, 2]
TamOr2 <- TamO.PM2.combo[PMIDs, 1] # ya que el identificador son los nombre de filas,
                                   # se puede usar poara reordenar- extraemos solo la primera columna
                                   # los tamaños
all(as.integer(TamOr2) == TamOr) # estan corectamente ordenados
# si quitas las lineas didacticas, la cantidad de codigo es igual, y acaba en 1/100 del tiempo.
# eso quiere decir que igual el bucle es intuitivo, pero no necesariamente lo mas eficiente
# 1a prioridad: que funciona; 2a: que tu y otros lo entienden; 3a: que sea eficiente

# se separa en grupos usando el sintaxis de forumas en R
# y ~ x [aqui y agrupado por x]
boxplot(EdadMedEmig ~ TamOr)

# modificamos el boxplot con mas parametros:
boxplot(EdadMedEmig ~ TamOr, 
        col = "lightblue", 
        varwidth = TRUE)    # ancho proporcional a la raiz cuadrada del numero de casos (municipios, no personas)
        
# mira ?boxplot para mas control

# en vez de boxplots de las edades media, hacemos boxplots de las edades tal cual,
# segun tamaño:
boxplot(EdadExacta ~ TAMUBAJA, data = EVRD, col = "lightblue", varwidth = TRUE)
# el anterior era mas descriptivo...

# ----------------------------------------------
# ejercicios:
# 1) calcular la edad media al emigrar (o imigrar) por municipios y sexo.
# 2) replicar el penultimo boxplot separando por sexo [preguntas aqui a la Anna Turu]

# algo mas grande (dividir y conquistar):
# 3) coger una serie completa de microdatos del MNP del INE y convirtelos todos a 'data.frame's, 
#    guardados como .Rdata [save()]. Mejor preferir los microdatos mas 
#    detallados que tiene el CED (pregunta Tere)- pero complementelos con los datos de la pagina
#    del INE. Divide el trabajo entre vosotros para que el CED tenga una copia de la casa en formato
#    R binario, que es igual lo mas conveniente (estan comprimidos, pero se lee mucho mas rapido). 
#      a) las fechas de nacimiento y evento esten calculadas y guardadas como columnas de class 'Date'. 
#      b) que luego hay columnas para edad (integer) y edad exacta (numeric)
#      c) las etiquetas de las columnas esten harmonizadas tanto posible (cambian variables, etc) 
#      d) todos los 'codigos' esten de class "character", para no perder los 0s
#      e) que los municipios siempre tengan un identificador unico que es la concatenacion
#         del codigo provincia y el codigo municipio. Esto no solo ayuda en calcular cosas por municipios
#         sino tambien adjuntar datos a mapas.
#    El trabajo esta facilitado por el hecho de que solo hay pocas revisiones de los ficheros de 
#    microdatos- 4 creo normalmente. Si cada persona hace 1 o 2 revisions, ya lo tenemos todo y se podra
#    hacer cosas mas grandes con aquello [en la session 5, si a caso]. Muchos fichero que tiene el CED
#    ya estan fuera del formato de ancho fijo- estos se podrian convertir a .csv en SPSS o lo que sea
#    y despues leer incialmente usando read.csv()- los que no tenga el CED con read.fwf()


#






