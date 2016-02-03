#===========================================================
#   Distribucion de los federados de la FVAS
#===========================================================


#======================= Introduccion ====================== 
#
# Objetivo
#------------------------------------------------------------
#    Representar sobre un mapa de las comarcas del Pais Vasco
#    el número de federados en el año 2015. La idea es hacer
#    dos mapas. Uno con los datos brutos, y otro en forma de
#    porcentaje sobre la población total de la comarca.
#
# Origen de los datos
#------------------------------------------------------------
#    Los datos con el numero de federados provienen del registro
#    de licencias de FVAS. Los datos originales han sufrido una 
#    fuerte depuración en la nomenclatura de los municipios.
#
#    La nomenclatura oficial de municipios y comarcas se ha obtenido
#    de http://www.geo.euskadi.eus/codigos-territoriales-de-euskadi-municipios-comarcas-y-entidades-de-poblacion-eustat/s69-geodir/es/
#
#    Los datos de población (2015), provienen de la pagina del INE
#    http://www.ine.es/jaxiT3/Tabla.htm?t=2905&L=0

# Origen de los mapas
#------------------------------------------------------------
#   http://www.geo.euskadi.eus/geonetwork/srv/spa/main.home
#   ftp://ftp.geo.euskadi.net/cartografia/Limites/
#
#========================================================== 


# Directorio de trabajo
setwd("C:/workdir")

# Cargar librerias
library(ggmap)            # Si
library(rgdal)            # Si
library(dplyr)            # para función rename
library(plyr)             # para función join

library(reshape)          # Si
library(RColorBrewer)     # Si


#------------------------------------------------------------
# Cargar el mapa
#------------------------------------------------------------

mapa<-readOGR(dsn="C:/workdir/mapas/Euskadi", "CB_COMARCAS_5000_ETRS89")


# Algunas comprobaciones sobre el mapa que hemos cargado

# Visualizar el mapa
plot(mapa)   

# Conocer sus componentes
slotNames(mapa)
## [1] "data" "polygons" "plotOrder" "bbox" "proj4string"

# Visualizar sus datos
head(mapa@data)
##                    COMARCA         COMARC_EUS COM_PROV COM_COMR
## 0       CANTABRICA ALAVESA  KANTAURI ARABARRA       01       06
## 1 ESTRIBACIONES DEL GORBEA   GORBEIA INGURUAK       01       05
## 2          LLANADA ALAVESA    ARABAKO LAUTADA       01       02
## 3         MONTAÃ‘A ALAVESA ARABAKO MENDIALDEA       01       03
## 4            RIOJA ALAVESA   ERRIOXA ARABARRA       01       04
## 5          VALLES ALAVESES    ARABAKO IBARRAK       01       01

# Las comarcas no tienen un código especifico. Es jerarquico respecto
# a la provincia. 


#------------------------------------------------------------
# Preparar el mapa para cargar los datos
#------------------------------------------------------------

# Covertir mapa en dataframe espacial
mapa2<-fortify(mapa, region=c("COMARCA"))

head(mapa2,2)
##       long     lat order  hole piece       group        id
## 1 551318.0 4780627     1 FALSE     1 ALTO DEBA.1 ALTO DEBA
## 2 551322.9 4780627     2 FALSE     1 ALTO DEBA.1 ALTO DEBA

str(mapa2)
# se observa que "id" es un "character". Lo convierto a "factor"

mapa2$id<-as.factor(mapa2$id)

# Ver los valores de "group" y "id" y escoger cual utilizar para enlazar 
# con los datos
unique(mapa2$group)
unique(mapa2$id)

# El campo "group" no parece apropiado. Para cada comarca hay más de un grupo. 
# Parece más adecuado utilizar "id". 

# Ver una lista de las comarcas
comarcas<-as.data.frame(unique(mapa2$id))
comarcas
##  1                 ALTO DEBA
##  2           ARRATIA-NERVION
##  3              BAJO BIDASOA
##  4                 BAJO DEBA
##  5        CANTABRICA ALAVESA
##  6    DONOSTIA-SAN SEBASTIAN
##  7              DURANGUESADO
##  8             ENCARTACIONES
##  9  ESTRIBACIONES DEL GORBEA
##  10           GERNIKA-BERMEO
##  11                  GOIERRI
##  12              GRAN BILBAO
##  13          LLANADA ALAVESA
##  14         MARKINA-ONDARROA
##  15         MONTAÃ‘A ALAVESA
##  16          PLENTZIA-MUNGIA
##  17            RIOJA ALAVESA
##  18                   TOLOSA
##  19              UROLA COSTA
##  20          VALLES ALAVESES


# Preparar el campo id para unir el mapa con los datos de licencias: Dos
# operaciones: pasar a minusculas y solucionar el problema de las "ñ".

# Pasamos a minusculas los nombres de las comarcas
mapa2$id<-tolower(mapa2$id)

# Hay un problema con la "ñ" de "montaña alavesa". En tanto averiguo como
# corregirlo en origen, lo más práctico es reemplazarlo en el actual DF.
mapa2$id <- gsub("montaã‘a", "montaña", mapa2$id)


# Visualizar el mapa con las comarcas
ggplot()+ 
  geom_polygon(data=mapa2, aes(x=long, y=lat, group=group, fill=id), color="white") +
  coord_fixed()

# Puede ser interesante poner el nombre de cada comarca
# Primero hay que crear un Df con los nombres y las coordenadas del centro de cada comarca
c_nombres <- aggregate(cbind(long, lat) ~ id, data = mapa2, FUN = function(x) mean(range(x)))
head(c_nombres)  

# visto en:
# https://trinkerrstuff.wordpress.com/2013/07/05/ggplot2-chloropleth-of-supreme-court-decisions-an-tutorial/

ggplot()+ 
 geom_polygon(data=mapa2, aes(x=long, y=lat, group=group, fill=id), color="white") +
 coord_fixed() +
 geom_text(data=c_nombres, aes(long, lat, label = id, angle=0, map_id =NULL), size=5)


#------------------------------------------------------------
# Cargar los datos
#------------------------------------------------------------

licencias2015 <- read.csv("~/Archivos JC/FVAS/FVAS 2015/datos_licencias_2015.csv", sep=";", dec=",")

str(licencias2015)
##  'data.frame':  18 obs. of  4 variables:
##  $ comarca  : Factor w/ 18 levels "Alto Deba","Arratia-Nervion",..: 14 10 4 17 16 6 18 11 3 15 ...
##  $ licencias: int  90 115 138 86 114 566 124 83 119 2 ...
##  $ poblacion: int  23085 41763 55752 41523 55568 327992 72104 51650 78426 1339 ...
##  $ ratio    : num  3.9 2.75 2.48 2.07 2.05 ...

# Las comarcas están en el campo "comarca". Es necesario renombrarlo como "id"
licencias2015 <- rename(licencias2015, id=comarca)

# Otro problema es que los nombres están en mayusculas en el mapa. Pasamos a 
# minusculas el campo "id" en ambos DF

licencias2015$id<-tolower(licencias2015$id)

# y por último, el problema de la ñ
licencias2015$id <- gsub("montaã‘a", "montaña", licencias2015$id)

# Comprobar el resultado
unique(licencias2015$id)


# Unir los datos
mapa2 <- join(x = mapa2, y = licencias2015, by = "id")


#------------------------------------------------------------
# Dibujar los mapas
#------------------------------------------------------------

# Ver las comarcas con su nombre

comarcas <- ggplot(mapa2, aes(x=long, y=lat, group = group)) + 
  geom_polygon(color="white", size=1, fill="gray75") +
  coord_fixed() +
  geom_text(data=c_nombres, inherit.aes=FALSE, aes(long, lat, label=id, angle=0, map_id=NULL), size=5)

# El argumento inherit.aes=FALSE es necesario para evitar errores. Al parecer, el
# argumento group que aparece en el primer componente del mapa queda "en memoria" 
# y al intentar representar los textos, la función lo busca y al no encontrarlo
# produce un error: "Error in eval(expr, envir, enclos) : object 'group' not found"
# inherit.aes=FALSE elimina esa memoria.Ver:
# http://stackoverflow.com/questions/10154293/geom-rect-failure-error-in-evalexpr-envir-enclos-object-variable-not-fo



# Crear mapas con datos

# Nº de licencias por comarca
ggplot(mapa2, aes(x=long, y=lat, fill = licencias, group = group)) + 
  geom_polygon(color="white", size=1) + 
  coord_fixed() +
  scale_fill_gradient(name="Nº de licencias", low = "bisque", high = "deeppink4") +
  theme(axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(legend.text = element_text(size = 14)) +
  ggtitle("Número de licencias") +
  theme(plot.title = element_text(face="bold", size=32)) +
  theme(legend.justification=c(0.9,0.1), legend.position=c(0.9,0.1)) +
  theme(panel.background = element_rect(fill = 'ivory4', colour = 'black')) +
  geom_text(data=c_nombres, inherit.aes=FALSE, aes(long, lat, label=id, angle=0, map_id=NULL), size=5)

# Ratio de licencias por comarca (licencias/1.000 habitantes)
ggplot(mapa2, aes(x=long, y=lat, fill = ratio, group = group)) + 
  geom_polygon(color="white", size=1) + 
  coord_fixed() +
  scale_fill_gradient(name="Lic./1.000 hab.", low = "bisque", high = "deeppink4") +
  theme(axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank()) +
  theme(legend.title = element_text(size=14, face="bold")) +
  theme(legend.text = element_text(size = 14)) +
  ggtitle("Número de licencias / 1.000 habitantes") +
  theme(plot.title = element_text(face="bold", size=32)) +
  theme(legend.justification=c(0.9,0.1), legend.position=c(0.9,0.1)) +
  theme(panel.background = element_rect(fill = 'ivory4', colour = 'black')) +
  geom_text(data=c_nombres, inherit.aes=FALSE, aes(long, lat, label=id, angle=0, map_id=NULL), size=5)




# Proximos pasos:
## ¿Es posible fabricar un "Theme" con todas las opciones de formato?



##### NOTAS 

# No se ve mucha diferencia entre las comarcas. Es debido a que
# la mayor parte de ellas tienen entre 0 y 150 federados, y el rango 
# total va de 0 a 854, por lo que la mayor de ellas se encuentran en
# el mismo intervalo. Hay que aumentar la definición de la escala.

# Aparece un problema:
## Error: cannot join on columns 'id' x 'id': index out of bounds 
# Es posible que sea debido a que hay dos comarcas que no tienen federados, 
# por lo que la longitud de los dos df es diferente.
# Las comarcas sin datos son "RIOJA ALAVESA" y "VALLES ALAVESES"
# Los añado manualmente al Df y vuelvo a cargar los datos
