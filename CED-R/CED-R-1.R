
# Author: triffe
###############################################################################

# R es una mega-calculadora:
# envia:
1+1

# haz flechitas para asignar ('=' tb funciona, pero mejor con flechas)
a <- 45
b <- .001 * log(2 * a) # funciones siempre usan parentesis
a+b

# log() se llama una 'function'. Hay miles de funciones. Funciones te hacen la vida facil.
# R es una lengua 'funcional'- eso quiere decir que todo se hace con funciones, y cada cosa
# puede estar 'funcionalizado'. Que es una funcion? En R todo es un objecto, funciones tambien.
# 

# ----------------------------------------------------------------------
# usar ayuda basica: 

# todas las funciones tienen ayuda. 
# La manera mas facil de ver la ayuda es usar un '?' frente de la funcion
?log
?plot

# el texto de ayuda tiene un formato estandar
# Destaco unas partes claves:

# Description: (de manera minimalista) que hace la funcion
# Arguments: cuales son los parametros de la funcion. Pueden ser obligatorios u opcionales 
#            esto se mira mucho. En vez de parametros, se llaman 'arguments'

# *Si sabes lo que quieres, pero no recuerdes bien bien el nombre del argumento, pues hay
#  una funcion para eso:
args(log)
args(optim) # optim() es un optimizador general (hay otros en R), parecido al solver de Excel

# Value: te dice lo que devuelve la funcion
# Details: a veces util
# Examples: lo mejor: si es una funcion nueva para ti, siempre va bien replicar los ejemplos
#           de l'ayuda:

# mira: (un ejemplo mas largo de replicar un ejemplo del 'help')
?plot

# vez la parte de ejemplos al fondo, aqui copiado y pegado:
# se puede enviar el codigo al R linea por linea o todo de golpe.
# para aprender mejor linea por linea para que veas que hace:
require(stats) # es para explicitar cual package esta usando
               # se suele usar library(stats) en vez de require(stats)
               # de hecho, ni hace falta cargar stats, como viene
               # en la instalacion basica...
plot(cars)     # 'cars' es un fichero de datos que se usa mucho en ejemplos
               # vez que plot() hace un scatterplot por defecto
lines(lowess(cars)) # aqui se pone una linea por encima (resulta de una regression no-parametrico)
# (me encanten los lowess() (tb loess()), hay que usar-los mas!

plot(sin, -pi, 2*pi) # see ?plot.function
               # menos relevante para demografos, pero para que veas que es capaz

## Discrete Distribution Plot:
plot(table(rpois(100,5)), type = "h", col = "red", lwd=10,
        main="rpois(100,lambda=5)")
# muchos argumentos nuevos!:
# rpois(100,5) produce 100 numeros aleatorios de la distribucion poisson con lambda = 5
# table() cuenta cuantas veces cada numero aparece
# en este caso plot() sabe que las etiquetas de table() son x y que las frequencias son y
# 'type' te permite decir que no sea un scatterplot. (compare con 'l', 's', 'o'...)
# 'col', color (caracter)
# 'lwd' amplitud de linea (line width)
# 'main' el titulo principal

## Simple quantiles/ECDF, see ecdf() {library(stats)} for a better one:
plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")

# vale? Eso quiere decir que puedes aprender mucho
# 1) replicando los ejemplos del help
# 2) jugando con argumentos- nada se va a romper!
# 3) vez que se puede superimponer sobre graficos- deseñar buen
#    buenos graficos en R es un proceso iterativo, vez probando, regenerando..

# otros recursos para help:
# a parte de Google, mira:

# http://stackoverflow.com/tags/r

# o busca un tutorio corto (largo) especifico (general)
# aqui: http://cran.r-project.org/other-docs.html

# o mira esto para una descripcion de las funciones mas frequentes:
# cran.r-project.org/doc/contrib/Short-refcard.pdf

# para saber cuales packages te pueden servir en una area especifica, los 'views':
# http://cran.r-project.org/web/views/

# o usa la lista de correo de RUGBCN, que es una fuente de ayuda local (preguntas generales)
# http://groups.google.com/group/rugbcn

# en general, empieza con ? y con Google.

# ----------------------------------------------------------------------
# uso basico de funciones:

# primero vamos a jugar un poco con funciones para que se ve como van.
# inventamos unos datos. 
x <- 1:10 # ':' es una funcion tambien!, es una abbreviacion de seq(from = 1, to = 10, by = 1)
          # ahora hemos asignado un objecto nuevo, se 'llama' x. 'x' contiene los
          # numeros 1,2...10 en una serie que se llama un 'vector'.
          # pienselo como una columna de Excel.
# si quieres ver que hay en x:
x # te lo pone en la parte de R
# y si x es muy grande y no quieres verlo todo?
head(x) # te muestra la primera parte
tail(x) # la ultima parte

(y <- rnorm(10)) # o si lo pones todo en parentesis tambien te lo muestra en el momento
# ahora tenemos varios objectos definidos:
ls() # nos dice todo lo que hay
# ya no vamos a usar 'a' y 'b'
# los quitamos:
rm(a,b) 
ls() # limpio

# ----------------------------------------------------
# packages:

# R tiene muchas funciones ya en la distribucion basica, pero lo bueno de R
# es que tiene MILES de packages aportados, todos gratuitos tambien...
# un package te da mas funciones, por si tienes que hacer algo mas especifico.

# para cargar un package hace falta 2 cosas:
# 1) installarlo: 
# install.packages() # da una lista impresionante
# install.packages("optimx") # si ya sabes lo que quieres
# 2) cargarlo: library(optimx)

# como sabes cual package tendra lo que necesitas? Google.

install.packages("optimx")
# una vez instalado, no hace falta instalarlo de nuevo, a no ser
# que actualices tu R, o que el package haya estado actualizado
# para actualizar todos tus packages:
update.packages()
# sueles tener mas packages que sabes, porque la mayoria de packages dependen de otros packages...

# ahora, para tener acceso a las funciones en un package (instalado):
library(optimx)
# ahora no vamos a usar nada en optimx, solo un ejemplo de cargar un package..

# tu sabras con el tiempo cuales packages sirven en tu trabajo especifico.
# hay hasta packages para demografos- 


# ----------------------------------------------------
# lectura e introduccion de datos:


