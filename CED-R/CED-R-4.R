
# Author: triffe
###############################################################################

# ancho del variable copiado y pegado del diseño registro en Excel
anchos <- c(3,  2,  2,  5,  1,  2,  2,  1,  1,  2,  2,  2,  1,  1,  2,  3,  1,  3,  2,  2, 
        2,  3,  1,  2,  1,  2,  3,  2,  1,  1,  1,  2,  2,  1,  1,  1,  2,  1,  1,  1,  2, 
        2,  2,  3,  3,  2,  3,  1,  2,  4,  4,  4,  1,  4,  4,  2,  1,  1,  1,  2,  4,  1,  
        1,  2,  2,  1,  1,  1,  1,  2,  1,  1,  2,  1,  1,  1,  3,  1,  1,  2,  1,  2,  2,  
        2,  1,  1,  1,  2,  3,  1,  2,  2,  7)

nom.var <- c('CICLO',   'CCAA', 'PROV', 'NVIVI',    'NIVEL',    'NPERS',    'EDAD5',    'RELPP1',  
        'SEXO1',    'NCONY',    'NPADRE',   'NMADRE',   'RELLMILI', 'ECIV1',    'PRONA1',   'REGNA1',   
        'NAC1', 'EXREGNA1', 'ANORE1',   'NFORMA',   'RELLB',    'EDADEST',  'CURSR',    'NCURSR',   
        'CURSNR',   'NCURNR',   'HCURNR',   'RELLB',    'TRAREM',   'AYUDFA',   'AUSENT',   'RZNOTB',  
        'VINCUL',   'NUEVEM',   'OCUP1',    'ACT11',    'SITU', 'SP',   'DUCON1',   'DUCON2',   
        'DUCON3',   'TCONTM',   'TCONTD',   'DREN', 'DCOM', 'PROEST',   'REGEST',   'PARCO1',   'PARCO2',   
        'HORASP',   'HORASH',   'HORASE',   'EXTRA',    'EXTPAG',   'EXTNPG',   'RZDIFH',   'TRAPLU',  
        'OCUPLU1',  'ACTPLU1 1',    'SITPLU',   'HORPLU',   'MASHOR',   'DISMAS',   'RZNDISH',  'HORDES',  
        'BUSOTR',   'BUSCA',    'DESEA',    'FOBACT',   'NBUSCA',   'ASALA',    'EMBUS',    'ITBU', 'DISP',
        'RZNDIS',   'EMPANT',   'DTANT',    'OCUPA*',   'ACTA* 1',  'SITUA*',   'OFEMP',    'SIDI1',    
        'SIDI2',    'SIDI3',    'SIDAC1',   'SIDAC2',   'MUN1', 'PRORE1',   'REPAIRE1', 'TRAANT',   'AOI',  
        'CSE',  'FACTOREL')

et.var <- c('Período de referencia',    'Comunidad autónoma',   'Provincia',    'Número de orden de la vivienda',   
        'Variable que indica el nivel del registro en el fichero',  'Número de la persona', 'Edad, grupos quinquenales de años cumplidos', 
        'Relación con la persona de referencia',    'Sexo', 'Número de persona del cónyuge',    'Número de persona del padre', 
        'Número de persona de la madre',    'Relleno antigua variable MILI',    'Estado civil legal',  
        'Si es en España, indicar provincia',   'Región del país extranjero de nacimiento', 'Nacionalidad', 
        'Región del país de la nacionalidad extranjera',    'Años de residencia en España', 'Nivel de estudios',   
        'Relleno en apartado de formación', 'Edad en la que alcanzó el máximo nivel de estudios',   'Cursa estudios reglados',  
        'Nivel de los estudios reglados que cursa', 'Cursa formación no reglada',   'Nivel de estudios de la formación no reglada', 
        'Total de horas de estudios no reglados en las últimas cuatro semanas ',    'Relleno en apartado de formación', 
        'Si ha realizado un trabajo remunerado durante la semana pasada',   'Ayuda familiar. Realización de trabajo no remunerado empresa familiar', 
        'A pesar de no haber trabajado ¿tenía un empleo o negocio?',    'Razones por las que no trabajo, teniendo empleo', 
        'Vinculación con el empleo de personas con empleo ausentes en la semana de referencia', 'Ha encontrado empleo ',   
        'Ocupación principal',  'Actividad principal',  '¿Cuál es su situación profesional (actividad principal)?', 'Tipo de administración en la que trabaja', 
        'Tiene contrato indefinido o temporal', 'Relación laboral de carácter permanente o discontinuo',    
        'Tipo de contrato o relación laboral de carácter temporal', 'Duración del contrato o rel. lab. si ha trabajado un mes o más', 
        'Nº de días del contrato o rel. lab. si ha trabajado menos de un mes',  'Tiempo en meses desde la renovación del contrato',
        'Tiempo en meses en la empresa',    'Provincia donde está ubicado', 'Región o País donde está ubicado', 'Tipo de jornada, completa o parcial', 
        'Motivo de tener jornada parcial',  'Horas pactadas en contrato o acuerdo de trabajo (hhmm)', 
        'Número de horas semanales que dedica a este trabajo habitualmente (hhmm)', 'Número de horas efectivas que dedicó a este trab. la semana pasada (hhmm)', 
        'Realizó horas extraordinarias en la semana de referencia', 'Número de horas extraordinarias pagadas realizadas en la semana de referencia (hhmm)', 
        'Número de horas extraordinarias no pagadas realizadas en la semana de referencia (hhmm)', 
        'Razón principal de la diferencia de horas efectivas y habituales', 'Si tiene otro u otros empleos',   
        '¿Cuál es la ocupación u oficio en el segundo empleo?', 'Actividad del establecimiento donde tiene el segundo empleo', 
        'Situación profesional en el segundo empleo',   'Nº de horas efectivas trabaj. la semana pasada en el segundo empleo (hhmm)',   
        'Si desearía trabajar más horas',   'Disponibilidad para trabajar más horas',   'Razones por las que no podría trabajar más horas', 
        'Número de horas que desearía trabajar habitualmente',  'Busca otro empleo o está haciendo gestiones para establecerse por su cuenta',  
        'Ha buscado empleo en las últimas cuatro semanas',  'Desearía tener un empleo', 'Métodos de encontrar empleo. Oficina de empleo de la admón.', 
        'Razones por las  que no busca empleo', 'El empleo que busca o ha encontrado es asalariado',    'Tipo de jornada en el empleo buscado / ha encontrado',
        'Tiempo que lleva buscando empleo / estuvo buscando empleo',    'Disponible para trabajar en un plazo de 15 días',
        'Razones para no poder empezar a trabajar en un plazo de 15 días',  'Si ha realizado antes algún trabajo', 
        'Número de meses transcurridos desde que dejó su último empleo',    'Ocupación u oficio que desempeñaba en su último empleo, si hace menos de un año que lo dejó', 
        'Actividad del establecimiento  donde trabajaba, si hace menos de un año que lo dejó', 
        'Situación profesional que tenía en su anterior trabajo, si hace menos de un año que lo dejó', 
        'Situación el domingo pasado, en relación con las of. Empleo de la admon.', 'Situación inactividad autopercibida en la que estaba la semana pasada', 
        'Situación inactividad autopercibida en la que estaba la semana pasada',    'Situación inactividad autopercibida en la que estaba la semana pasada',   
        'Situación de actividad autopercibida en la que estaba la semana pasada',   'Situación de actividad autopercibida en la que estaba la semana pasada', 
        'Lugar de residencia hace un año',  'Si es España: provincia',  'Región del país extranjero de  residencia anterior', 
        'Si trabajó en algún momento el año pasado',    'Clasificación de los entrevistados por relación con la actividad económica según criterios OIT',  
        'Asignación de la condición socioeconómica',    'Factor de elevación ')
# segundo trimestre 2012 EPA  
# file.choose() # para cambiar la direccion
EPA <- read.fwf("/home/triffe/git/CED-R/CED-R/data/EPAWEBT0212",
        widths = anchos,           # ancho columnas
        col.names = nom.var,       # etiquetas columnas
        colClasses = c(rep("character", (length(anchos) - 1)), "numeric"),  # tipo de dato (realmente hay algunos que son numericos o integer, pero esto es mas sencillo por ahora
        stringsAsFactors = FALSE,  # nada categorico (si lo queremos lo haremos en el momento)
        strip.white = TRUE         # quita espacios que no tienen sentido
)

dim(EPA)
prod(dim(EPA)) # uau
head(EPA)
# si no hay un valor, sera un missing
EPA[EPA == ""] <- NA
# FACTOREL habra que dividir por 100 (no hay un decimal en los datos originales)
EPA$FACTOREL <- EPA$FACTOREL / 100

# ahora- no vamos a poner etiquetas largas a las columnas, por lo que se puede hacer es una tabla de referencia para buscar 
# los variables que nos interesan:

var.ref <- cbind(nom.var, et.var)

# guardamos una copia (o igual si acabas limpiando otros variables en formatos mas utiles, guarda una copia en 
# el formato binario de R despues.
save(EPA, file = "/home/triffe/git/CED-R/CED-R/data/EPAWEBT0212.RData")
# recuerda, se puede cargar al espacio de trabajo con: load("/home/triffe/git/CED-R/CED-R/data/EPAWEBT0212.RData") # cambiando la direccion, claro

# ----------------------------------------------------------------------------------------------
# a continuacion vamos a hacer unas cosas que nos imagino el Joan

# 1) solo vamos a trabajar con unos 7 variables. Cortamos el fichero
var.guardar <- c("NPERS", "NVIVI", "NCONY", "EDAD5", "SEXO1", "AOI", "PROV", "FACTOREL")
all(var.guardar %in% nom.var) # comprobar que hemos escrito bien los nombres de los variables

EPA2 <- EPA[, var.guardar]
head(EPA2)

# 2)
# seleccionar casos de edades >= 15
EPA2$EDAD5 <- as.integer(EPA2$EDAD5)
EPA3 <- EPA2
EPA2 <- EPA2[EPA2$EDAD5 >= 15, ]

# 2) recodificamos el variables AOI para tener 3 categorias: (parados, ocupados e inactivos)
# los codigos copiado y pegado:
#03  Ocupados subempleados por insuficiencia de horas
#04  Resto de ocupados
#05  Parados que buscan primer empleo
#06  Parados que han trabajado antes
#07  Inactivos 1 (desanimados)
#08  Inactivos 2 (junto con los desanimados forman los activos potenciales)
#09  Inactivos 3 (resto de inactivos)

unique(EPA2$EDAD5)
unique(EPA2$AOI)

# nuestros codigos nuevos:
# 1: ocupado
# 2: parado
# 3: inactivo
nom.aoi3 <- c("ocupados","parados","inactivos")
EPA2$AOI3 <- with(EPA2, ifelse(AOI %in% c("03","04"), 1,
                          ifelse( AOI %in% c("05","06"), 2, 
                            ifelse( AOI %in% c("07","08","09"), 3, NA)
                )))
unique(EPA2$AOI3)
any(is.na(EPA2$AOI3))

# miramos cuantos casos tenemos en las 3 categorias nuevas:
(aoi3.tab <- table(EPA2$AOI3))
names(aoi3.tab) <- nom.aoi3 # para recordarnos


# 3 calcular la proporcion de parados (parados / (parados + ocupados) por edad, teniendo en cuenta que esta ponderado.
head(EPA2)

# asi lo podrias hacer manualmente para una edad, o edad por edad.
edad.y.parado <- EPA2$EDAD5 == 16 & EPA2$AOI3 == 2
edad.y.activo <- EPA2$EDAD5 == 16 & EPA2$AOI3 %in% c(1, 2)
parados.edad.16.19 <- sum(EPA2$FACTOREL[edad.y.parado]) / sum(EPA2$FACTOREL[edad.y.activo])
# no hay nada incorecto en hacerlo asi, pero te duraria mucho tiempo, y seria mucho codigo

# haremos todas las edads a la vez:
# 1) en un bucle (para Amand)
edades <- sort(unique(EPA2$EDAD5))
paro.edad <- vector(length = length(edades))
for (i in 1:length(paro.edad)){
    edad.y.parado <- EPA2$EDAD5 == edades[i] & EPA2$AOI3 == 2
    edad.y.activo <- EPA2$EDAD5 == edades[i] & EPA2$AOI3 %in% c(1, 2)
    paro.edad[i] <- sum(EPA2$FACTOREL[edad.y.parado]) / sum(EPA2$FACTOREL[edad.y.activo])
}

plot(edades, paro.edad, type = "s")

# look for an elegant R way of doing this with no for loop
#library(reshape2)
#
#install.packages("weights")
#library(weights)
#test <- c(1,1,1,1,1,1,2,2,2,3,3,3,4,4)
#weight <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)
#wpct(test)
#wpct(test, weight)
#tab <- table(test)
#tab /  sum(tab)
#
#test <- wpct(EPA2$AOI3, EPA2$FACTOREL)
#test[2] / sum(test[1:2])
#
#library(plyr)
#?ddply
#ddply(xx,.(value),summarise, wm = weighted.mean(avg,weight))
#
#


# 3) hacemos una piramide bontia de parados y ocupados superpuestos:
# solucion para windows
source(pipe(paste("wget -O -", "http://raw.github.com/timriffe/Pyramid/master/Pyramid/R/Pyramid.R")))

library(reshape2)
head(EPA2)
tab.pir <- acast(EPA2, EDAD5 ~ SEXO1 + AOI3, sum, value.var = "FACTOREL")

widths <-  c(4, rep(5, nrow(tab.pir ) - 1))
# etiquetas siguen SEXO1_AOI3

# asi que 1_1 = varones ocupados; 6_1 mujeres ocupadas

# hay que hacer trampa con Pyramid()- el calcula edades a partir de los anchos
# asi que quiere empezar con 0...
age.at <- as.integer(rownames(tab.pir)) - 16
age.lab <- paste(rownames(tab.pir), seq(19, 69, by = 5), sep = "-")

# primero ocupados:
Pyramid(males = tab.pir[,"1_1"], females = tab.pir[,"6_1"],  
        widths = widths,    # anchos de las barras (la primera es diferente a los demas)
        age.at = age.at,    # donde ponemos las etiquetas de edad (suponiedo que empiezan con 0)
        age.lab = age.lab,  # las etiquetas de edad
        grid = FALSE,        # igual que age.lines = FALSE y v.lines = FALSE
        main = "Estructura de edades de parados y ocupados\nEspaña 2o trimestre 2012 (EPA)",
        xax.lab = c("2%","1,5%","1%","0,5%","", "0,5%","1%","1,5%","2%"),
        fill.males = "lightblue",
        fill.females = "lightblue"
)
par(new = TRUE)
Pyramid(males = tab.pir[,"1_2"], females = tab.pir[,"6_2"],  
        widths = widths,    # anchos de las barras (la primera es diferente a los demas)
        age.at = age.at,    # donde ponemos las etiquetas de edad (suponiedo que empiezan con 0)
        age.lab = age.lab,  # las etiquetas de edad
        grid = FALSE,        # igual que age.lines = FALSE y v.lines = FALSE
        fill.males = NA,
        fill.females = NA,
        border.males = gray(.3),
        border.females = gray(.3),
        main = "",
        xax.lab = rep("",9)
)

# be carefully to explicitly define xlim and ylim for both plots so they match dimensions perfectly
par(new=TRUE)
Pyramid(males = PTpop[,3], females = PTpop[,4],)


# 4) recodificar algunos variables

# la manera classica- con ifelse() - como un ifelse() en excel
EPA2$AOI3 <- with(EPA2, ifelse(AOI %in% c("03","04"), 1,
                        ifelse( AOI %in% c("05","06"), 2, 
                        ifelse( AOI %in% c("07","08","09"), 3, NA)
                )))
# esto puede tener sentido si no hay muchas combinaciones, como aqui
# pero si tienes muchas combinaciones es bastante trabajoso
unique(EPA2$AOI3)
EPA3$AOI3 <- EPA2$AOI3 # lo guardamos en el otro tb
# otra manera, mas rapido, de recodificar un variable (sobre todo si sea complejo o con muchos valores):

# un truco en R que se me ocurre para la recodificacion compleja:
a <- c(g=1,h=2,i=7)
a[c("g","g","g","g","g","i","i")] # veas que se puede repetir el mismo nombre 
                          # y que el vector resultado puede superar la 
                          # longitud de la primera vector?
# explotamos esta propiedad para recodificar muchas cosas, y nos salvamos trabajo

# hace falta una table de equivalencia. mejor que escribirla a mano es importar una, 
# como son faciles de encontrar. siempre salvete trabajo!
provCA <- read.table("/home/triffe/git/CED-R/CED-R/data/PROV_CA.txt", 
        sep = "\t", 
        fileEncoding = "utf8", # acentos, aunque no usamos los nombres..
        header = FALSE, 
        col.names = c("ProvName","codiprov","codiCA"),
        stringsAsFactors = FALSE)
head(provCA)  # codigos estan malos: necesitan "01" en vez de 1
# codigo exotico para lograrlo:
provCA$codiprov <- sprintf("%.2d", provCA$codiprov) # ponemos los 0s delante (convirte a character)
provCA$codiCA   <- sprintf("%.2d", provCA$codiCA)
head(provCA) # mejor

# ahora queremos un vector de los codigos de CCAA, con etiquetas que son los codigos de provincia
CCAA <- provCA$codiCA
names(CCAA) <- provCA$codiprov # esto sera nuestra herramienta para recodificar

CCAA[c("02","02","03")]

all(EPA2$PROV %in% names(CCAA)) # a ver si saldra limpio
# qualquier otra manera (y hay muchas) de recodificar esto seria chungo.
EPA2$CA <- CCAA[EPA2$PROV] # puedes verificar que haya hecho lo corecto
head(EPA2)

# 5) boxplots parados edad (distribucion entre provincias)
library(reshape2) # me encanta ese package (viene ya con la installacion base)
# tabla de parados (ponderado) por sexo y edad y provincia
tab.boxn <- acast(EPA2[EPA2$AOI3==2,], EDAD5 ~ SEXO1 + PROV, sum, value.var = "FACTOREL")
# tabla de activos por sexo y edad  y provincia
tab.boxd <- acast(EPA2[EPA2$AOI3 %in% c(1,2),], EDAD5 ~ SEXO1 + PROV, sum, value.var = "FACTOREL")
# proporcion parado por sexo y edad  y provincia
tab.box <- tab.boxn / tab.boxd
tab.box.v <- tab.box[, 1:52]    # varones
tab.box.m <- tab.box[, 53:104]  # mujeres

# puedes ver los graficos uno al lado del otro
par(mfrow = c(1,2))
boxplot(t(tab.box.v), main = "prop varones parados por edad\ndistribucion entre provincias")
boxplot(t(tab.box.m), main = "prop mujeres parados por edad\ndistribucion entre provincias")
# y gastar tiempo haciendolos bonito..

# o puedes ponerlos al lado dentro del mismo grafico, para que sea mas facil de comparar:
xpos <- sort(unique(EPA2$EDAD5))
boxplot(t(tab.box.v), main = "prop varones parados por edad\ndistribucion entre provincias", at = xpos - .75, width = rep(.45,length(xpos)), xlim = c(14,68),
        col = "lightblue")
boxplot(t(tab.box.m), main = "", at = xpos + .75, width = rep(.45,length(xpos)), xlim = c(14,68), 
        axes = FALSE, add = TRUE, col = "pink")

# 6) añadir variables del hogar a los individuos
# con algunos variables aggregados de los individuo miembros:
# 1) nr total de personas > 16 
# 2) nr ocupados
# 3) nr parados
# 4) nr inactivos

# trabajando con EPA3 (el 3 tenia los menores tambien)
EPA3$AOI3[is.na(EPA3$AOI3)] <- 4 # damos un valor de AOI3 que no sea NA a los menors
# hacemos frequencias de AOI3 para cada hogar
hogares <- table(EPA3$NVIVI, EPA3$AOI3)
head(hogares) # para que veas que es: las columnas son los 4 valores posibles de AOI3..

# añadimos el numero total de personas
hogares <- cbind(hogares, rowSums(hogares))

# añadimos el numero total de personas > 15
hogares <- cbind(hogares, hogares[,5]-hogares[,4])

# etiquetas de columnas mas utiles:
colnames(hogares) <- c("Hocupados","Hparados","Hinactivos","Hnr<16","Htotal","Hnr>16")

# usamos el mismo truco de 'recodificacion' para añadir los hogares a los individuos
# ojo: hogares tiene menos filas que EPA3. Las etiquetas de fila son los codigos de hogar
# por lo tanto, si seleccionamos filas hogares, usando la columna del codigo de hogar en 
# EPA3, nos repite fila como sea necessario, y nos las ordena tambien: perfecto!
EPA3      <- cbind(EPA3, hogares[EPA3$NVIVI, ]) # por la tanto, el siguiente funciona sin error
head(EPA3) # ahora con unas columnas mas

# calculamos un indice de dependencia dentro del hogar:
EPA3$Hdep <- (EPA3$Htotal -  EPA3$Hocupados) / EPA3$Htotal


# 7) un fichero de parejas:

# primero definimos identificadores unicos de individuos y parejas (NCONY)
EPA2$IDper        <- paste0(EPA2$NVIVI, EPA2$NPERS)
EPA2$IDperCONY    <- paste0(EPA2$NVIVI, EPA2$NCONY)
# confirmamos que los ID sean unicos:
length(unique(EPA2$IDper)) == nrow(EPA2) 

# seleccionamos mujeres y hombres en pareja, en objecto aparte
EPA2h             <- EPA2[EPA2$SEXO == "1" & EPA2$NCONY != "00",]
EPA2d             <- EPA2[EPA2$SEXO == "6" & EPA2$NCONY != "00", ]
# modificamos las etiquetas de columna:
colnames(EPA2d)   <- paste0("D", colnames(EPA2d) )
# etiquetamos las filas usando los ID de las personas (mujeres)
rownames(EPA2d)   <- EPA2d$DIDper
head(EPA2d)

# la estrategias es de usar 
parejas <- cbind(EPA2h,  # pegamos los hombres en pareja
                 EPA2d[EPA2h$IDperCONY, ]) # a las mujeres en pareja (seleccionadas por las parejas de los hombres-
                                           #                          que coinciden con las etiquetas de fila de las mujeres)
# el producto deseado:
head(parejas)

