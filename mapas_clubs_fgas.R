#============================================================
#   Mapa de los clubes de la FVAS
#============================================================


#======================= Introduccion ======================= 
#
# Objetivo
#------------------------------------------------------------
#    Representar sobre un mapa los clubes pertenecientes a
#    la FVAS en el año 2015.
#
#    La idea es hacer varios mapas, usando como base tanto una
#    imagen de Google Maps, como un shp de las comarcas del 
#    Pais Vasco.
#
# Origen de los datos
#------------------------------------------------------------
#    Los datos con los clubes y el numero de federados provienen 
#    del registro de licencias de FVAS. Los datos originales han 
#    sufrido una fuerte depuración en la nomenclatura de los 
#    municipios.
#
#    La nomenclatura oficial de municipios y comarcas se ha obtenido 
#    de http://www.geo.euskadi.eus/codigos-territoriales-de-euskadi-municipios-comarcas-y-entidades-de-poblacion-eustat/s69-geodir/es/
#
# Origen de los mapas
#------------------------------------------------------------
#   http://www.geo.euskadi.eus/geonetwork/srv/spa/main.home
#   ftp://ftp.geo.euskadi.net/cartografia/Limites/
#
#============================================================ 


# Directorio de trabajo
setwd("C:/workdir")

# Cargar librerias
library(ggmap)            # Si 
library(rgdal)            # Si


library(RColorBrewer)


#------------------------------------------------------------
# Cargar la lista de clubs
#------------------------------------------------------------

clubes_2015 <- read.csv("~/Archivos JC/FVAS/FVAS 2015/clubes_2015.csv", sep=";", dec=",")

names(clubes_2015)
## [1] "Localidad" "todo"      "Numero"     "Provincia"

#   "Localidad"   es el municipo donde está el club
#
#   "todo"        es una cadena de texto con el municipio y el pais, 
#                 que se utilizará para la geolocalización del municipio. 
#                 Es importante que esté entre comillas, por ejempl: 
#                 "EIBAR, SPAIN". 
#
#   "Numero"      es el número de clubs en cada localidad
#
#   "Provincia"   provincia a la que pertenece la localidad del club


# Obtener las coordenadas de cada municipio
#------------------------------------------------------------

# Se trata de preparar un DF con las coordenadas de cada municipio.
# Es muy sencillo hacerlo de forma individual con la función geocode

geocode("EIBAR, SPAIN", source="google")
##          lon      lat
##  1 -2.471542 43.18321

# Sin embargo, resulta más difícil aplicar la función a una estructura
# de datos compleja (data frame, matriz, vector...) y obtener esa
# misma estructura ordenada con la información en su interior.
# Por ello hay que construir una expresión que utiliza cuatro 
# funciones diferentes:

# Preparar un DF, llamado "coor", con las coordenadas de cada municipio:

coor <- do.call(rbind,
                lapply(1:nrow(clubes_2015),
                function(i)geocode(as.character(clubes_2015[i, 2]), source="google")))

# Podemos revisar el objeto creado

class(coor)     # [1] "data.frame"
names(coor)     # [1] "lon" "lat"
head(coor,3)
##           lon      lat
##    1 -2.897002 43.43211
##    2 -2.891265 43.23532
##    3 -2.997398 43.36007

# ------------- Explicación de la función ------------- 

# do.call {base} es una función que aplica y ejecuta otras funciones   
# sobre una lista de argumentos.
# Ver https://stat.ethz.ch/R-manual/R-devel/library/base/html/do.call.html

# rbind {base} une vectores, matrices o data frames añadiéndolos como filas.

# lapply {base} aplica una función a los elementos de una lista o vector, 
# y devuelve los resultados en forma de lista.
# ver https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

# geocode {ggmap} busca las coordenadas de un determinado punto, ya sea  
# una ciudad, un país, un ediicio.... la condición es que google maps 
# lo pueda encontrar, porque en realidad es google maps quien lo busca. 
# El parámetro source="google" es ahora necesario, pero no en el momento 
# en que se publicó la función, por lo que los scripts que hay en muchas 
# páginas web y repositorios, podrían no funcionar.
# Antes de aplicar la función, es necesario asegurarse que la dirección  
# que se va a procesar sea un texto. Por ello se ha incorporado la función 
# "as.character" antes de la referencia al DF con los datos. En caso contrario, 
# puede aparecer un mensaje de error:"Error: is.character(location) is not TRUE"

# En conjunto, la función lapply aplica la función geocode a la columna 2 de  
# cada fila del DF clubes_2015. El número de filas se obtiene de la expresión 
# "nrow(clubes_2015)", por lo que las filas a procesar serán "1:nrow(clubes_2015)"
# La función rbind va uniendo esas filas a  medida que van siendo calculadas, 
# en el mismo orden en que están en el DF del que procede la información de los 
# municipios. La función do.call permite que estas operaciones se realicen de 
# forma ordenada.


# Unir las coordenadas de cada municipio a los datos originales
#------------------------------------------------------------

clubes_2015<-cbind(clubes_2015, coor)

head(clubes_2015, 2)
##   Localidad              todo Numero       lon      lat
## 1  ARMINTZA "ARMINTZA, SPAIN"      1 -2.897002 43.43211
## 2   BASAURI  "BASAURI, SPAIN"      1 -2.891265 43.23532



# Preparar un mapa del País Vasco con la posición de los clubes
#--------------------------------------------------------

# 1. Preparar un mapa base
euskadi <- ggmap(get_map(location = "Eibar, SPAIN", maptype = "road", zoom = 9))

# Se podría haber escogido "euskadi" como "location", pero al utilizar 
# "Eibar, SPAIN", se obtiene un mapa más centrado.

# Hay más opciones para configurar este mapa base. 
# Ver opciones en la documentación de {ggmap}. Otra referencia muy útil es
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
# Por ejemplo:  

euskadi2<-ggmap(get_map(location = "Eibar, SPAIN", maptype = "satellite", zoom = 9))


# Añadir los puntos

euskadi+
  geom_point(data=clubes_2015, aes(x=lon, y=lat), color="red", size=clubes_2015$Numero)

  
# Los puntos correspondientes a las localidades con solo un club son
# muy pequeños. Convendría aumentarlos de tamaño.

# Una opción es utilizar la función "scale_size". Es necesario poner primero
# el atributo "size" dentro del grupo "aes". Si esto no se hace, el tamaño de 
# los puntos no responde al atributo. Por ensayo - error se ha buscado el rango 
# de tamaños más equilibrado (de 4 a 12).

euskadi +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero), color="red") +
  scale_size(range = c(4, 12)) +
  theme_nothing(legend = TRUE) +
  theme(legend.title = element_text(size=14, face="bold")) +
  ggtitle("Número de clubes de la F.V.A.S.") +
  theme(plot.title = element_text(size=20, face="bold"))

# Nota: Si los atributos del título del gráfico se ponen antes que los 
# de la leyenda, no funcionan.

# Una Variante es cambiar el color del punto según la provincia

euskadi +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero, color=Provincia) ) +
  scale_size(range = c(4, 13)) +
  theme_nothing(legend = TRUE) +
  theme(legend.title = element_text(size=14, face="bold")) +
  ggtitle("Número de clubes de la F.V.A.S.") +
  theme(plot.title = element_text(size=20, face="bold"))

# El problema es que los colores no son muy llamativos. Hace falta 
# configurarlos de forma manual. Tal vez la mejor solución sea
# independizar cada serie.

euskadi +
  geom_point(data=clubes_2015[clubes_2015$Provincia=="Bizcaia", ], aes(x=lon, y=lat, size=Numero), color="red") +
  geom_point(data=clubes_2015[clubes_2015$Provincia=="Gipuzkoa", ], aes(x=lon, y=lat, size=Numero), color="blue") +
  geom_point(data=clubes_2015[clubes_2015$Provincia=="Araba", ], aes(x=lon, y=lat, size=Numero), color="green") +
  scale_size(range = c(4, 13)) +
  theme_nothing(legend = TRUE) +
  theme(legend.title = element_text(size=14, face="bold")) +
  ggtitle("Número de clubes de la F.V.A.S.") +
  theme(plot.title = element_text(size=20, face="bold"))


##### --------------------------------------------------------------!!!
##### Falta: escala de la leyenda sin decimales
##### --------------------------------------------------------------



# Preparar un mapa con la posición de los clubes
# sobre un mapa de las comarcas del País Vasco
#--------------------------------------------------------

# Ahora se trata de representar los clubes sobre un mapa con las comarcas
# del Páis Vasco. El mapa de origen está en formato SHP.


# Cargar el mapa de partida
mapa_comarcas<-readOGR(dsn="C:/workdir/mapas/Euskadi", "CB_COMARCAS_5000_ETRS89")

# Obtener algo de información acerca del mapa cargado

class(mapa_comarcas)
##  [1] "SpatialPolygonsDataFrame"
str(mapa_comarcas)
names(mapa_comarcas)
##  [1] "COMARCA"    "COMARC_EUS" "COM_PROV"   "COM_COMR" 
proj4string(mapa_comarcas)
## [1] NA
dimensions(mapa_comarcas)
## [1] 2
summary(mapa_comarcas)
## Object of class SpatialPolygonsDataFrame
## Coordinates:
##   min       max
## x  463435.2  603082.2
## y 4702192.1 4811578.7
## Is projected: NA 
## proj4string : [NA]
## Data attributes:
##   COMARCA                COMARC_EUS COM_PROV    COM_COMR
## ALTO DEBA         : 1   ARABAKO IBARRAK   : 1   01  :6   01     :3  
## ARRATIA-NERVION   : 1   ARABAKO LAUTADA   : 1   20  :7   02     :3  
## BAJO BIDASOA      : 1   ARABAKO MENDIALDEA: 1   48  :7   03     :3  
## BAJO DEBA         : 1   ARRATI-NERBIOI    : 1   NA's:1   04     :3  
##  CANTABRICA ALAVESA: 1   BIDASOA BEHEREA   : 1            05     :3  
##  (Other)           :15   (Other)           :15            (Other):5  
##  NA's              : 1   NA's              : 1            NA's   :1  

plot(mapa_comarcas)

# Nuestro mapa no tiene definido el sistema de proyección.
# Esto no es un impedimento para trabajar con el, pero hace
# muy difícil el combinarlo con otros datos georeferenciados,
# como la posición de los clubes que hemos utilizado en el
# mapa anterior. Vamos a verlo.

# Preparar un primer mapa

mapa_comarcas_1<-fortify(mapa_comarcas, region=c("COMARCA"))
str(mapa_comarcas_1)
names(mapa_comarcas_1)
##  'data.frame':  197950 obs. of  7 variables:
##  $ long : num  551318 551323 551332 551342 551353 ...
##  $ lat  : num  4780627 4780627 4780627 4780628 4780627 ...
##  $ order: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ hole : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ piece: Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ group: Factor w/ 49 levels "ALTO DEBA.1",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ id   : chr  "ALTO DEBA" "ALTO DEBA" "ALTO DEBA" "ALTO DEBA" ...

# Podemos comparar los valores de las coordenadas long/lat de los 
# poligonos de este mapa, con las de los puntos proporcionados por
# la función geocode. Por ejemplo, Eibar tenía lon = -2.471542  y
# lat = 43.18321. Nada que ver. 
# Si a pesar de todo, intentamos hacer el mapa...

ggplot()+ 
  geom_polygon(data=mapa_club_2, aes(x=long, y=lat, group=group, fill=id), color="white") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat), color="red", size=clubes_2015$Numero)

# La imagen que resulta parece no tener sentido, pero si nos fijamos bien,
# el mapa con las comarcas está en la esquina superior derecha, y los puntos,
# en la esquina inferior izquierda. Este mismo problema se comenta en 
# http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/

# El archivo zip con el shp no incorpora ningún metadato, pero en la web 
# de origen se puede leer que el Sistema de Referencia Espacial es 
# ETRS89-UTM30N [EPSG:25830], que es por cierto parte del nombre del archivo.
# Más información sobre ETR98 en https://es.wikipedia.org/wiki/ETRS89

# Tal vez lo primero sea informar el mapa con el sistema de coordenadas utilizado.
# Parece que para ello hay que usar la función CRS


# Intentamos informar el sistema de proyección
# en https://freshbiostats.wordpress.com/2014/02/16/spatial-objects-in-r-i/

proj4string(mapa_comarcas) <- CRS("+proj=utm +zone=30 +ellps=WGS84")
mapa_comarcas@proj4string
## CRS arguments: +proj=utm +zone=30 +ellps=WGS84

# Convertir al sistema de Google Maps (lon, lat)
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

mapa_comarcas_gps = spTransform(mapa_comarcas, CRS("+proj=longlat +ellps=GRS80"))
summary(mapa_comarcas_gps)

# Probar el mapa

mapa_comarcas_2<-fortify(mapa_comarcas_gps, region=c("COMARCA"))

# Comarcas y puntos
ggplot()+ 
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group, fill=id), color="white") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat), color="red", size=clubes_2015$Numero)

# Con Google map de fondo
euskadi+ 
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group, fill=id), color="white") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat), color="red", size=clubes_2015$Numero)


# Hay algunos problemas. Hay polígonos que parecen mal cerrados, pero solo si está
# el mapa de Google de fondo.
# Esto es debido a que la base de Google map es más pequeña que el mapa de poligonos
# y parte de los puntos quedan fuera del mapa.

# Generamos un mapa más centrado
euskadi3<-ggmap(get_map(location = "Arrasate, SPAIN", maptype = "road", zoom = 9))

# Repetimos la combinación de mapas
euskadi3 + 
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group, fill=id), color="white") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat), color="red", size=clubes_2015$Numero)

# Ahora la comarca más al sur se dibuja correctamente, pero el problema se
# mantiene en la más occidental. Y esto no se puede arreglar cambiando el
# centro del mapa.

# Una opción es personalizar las dimensiones del mapa.
# Necesitamos conocer las coordenadas máximas y mínimas del mapa SHP:

summary(mapa_comarcas_2$long)
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -3.450  -2.712  -2.188  -2.345  -2.023  -1.729
summary(mapa_comarcas_2$lat)
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   42.47   43.11   43.29   43.21   43.32   43.46 

# Con estos datos configuramos el mapa que buscamos, teniendo en cuenta que hay 
# que introducirlas en este orden: min-long, min-lat, max-long, max-lat.
# Ver: http://www.4byte.cn/question/646949/creating-base-layer-for-ggmap-not-recognizing-data-frame.html

euskadi3 <- ggmap(get_map(location = c(-3.45, 42.47, -1.729, 43.46), source="google", maptype="road"))

# Aunque mejor es dejar un poco de margen
euskadi3 <- ggmap(get_map(location = c(-3.6, 42.3, -1.6, 43.6), source="google", maptype="road"))

# Juntamos el mapa base, las comarcas y los puntos
euskadi3 + 
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group, fill=id), color="white") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat), color="red", size=clubes_2015$Numero)


# Acabado final
#====================================================================

# Es posible mejorar la imagen final

# Aumentar la transparencia del SHP y el tamaño de los puntos
# Agregar nombres de las comarcas, títulos...


# Preparar los nombres de las comarcas
c_nombres2 <- aggregate(cbind(long, lat) ~ id, data = mapa_comarcas_2, FUN = function(x) mean(range(x)))
c_nombres2$id <- gsub("MONTAÃ‘A ALAVESA", "MONTAÑA ALAVESA", c_nombres2$id)

euskadi3 +
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group), alpha=0.2, color="black", fill="blue") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero), color="red") +
  scale_size(range = c(4, 13)) +
  geom_text(data=c_nombres2, aes(long, lat, label = id, angle=0, map_id =NULL), face="bold", size=5, color="blue") +
  ggtitle("Clubes de la F.V.A.S.") +
  theme(plot.title = element_text(face="bold", size=32))

# Demasiados nombres. hay que currarselo más

# Cambiando el mapa base y eliminando los nombres de las comarcas
euskadi4 <- ggmap(get_map(c(-3.5, 42.4, -1.65, 43.5), source="osm"))
euskadi4 +
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group), alpha=0.1, color="black", fill="blue") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero), color="red") +
  scale_size(range = c(4, 13)) +
  ggtitle("Clubes de la F.V.A.S.") +
  theme(plot.title = element_text(face="bold", size=32))

euskadi5<-ggmap(get_map("Euskadi", source="stamen", zoom=9,  maptype = "toner-2011")) 
euskadi5 +
  geom_polygon(data=mapa_comarcas_2, aes(x=long, y=lat, group=group), alpha=0.1, color="black", fill="blue") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero), color="red") +
  scale_size(range = c(4, 13)) +
  ggtitle("Clubes de la F.V.A.S.") +
  theme(plot.title = element_text(face="bold", size=32))

# ¿y cambiándo el color de la comarca según la provincia?

# De forma manual:

# 1. Preparar un Df con las comarcas

id<-unique(mapa_comarcas_2$id)
id<-as.data.frame(id)
names(id)
## [1] "id"

# 2. Preparar un vector con la provincia correspondiente a cada comarca

provincia<-c("Gipuzcoa", "Bizcaia", "Gipuzcoa", "Gipuzcoa", "Bizcaia",
             "Gipuzcoa", "Bizcaia", "Bizcaia", "Bizcaia", "Bizcaia", 
             "Gipuzcoa", "Bizcaia", "Araba", "Bizcaia", "Araba", 
             "Bizcaia", "Araba", "Gipuzcoa", "Gipuzcoa", "Araba")
provincia<-as.data.frame(provincia)
names(provincia)
## [1] "provincia"

# 3. Unir los dos DF en uno solo (comarca_provincia)
comarca_provincia<-cbind(id, provincia)
head(comarca_provincia)
##                       id provincia
##   1            ALTO DEBA  Gipuzcoa
##   2       ARRATIA-NERVION  Bizcaia
##   3         BAJO BIDASOA  Gipuzcoa
##   4            BAJO DEBA  Gipuzcoa

# 4. Utilizar este DF para informar de la provincia en mapa_comarcas_2

prueba <- mapa_comarcas_2
prueba <- join(x = prueba, y = comarca_provincia, by = "id")
head(prueba)
##         long      lat order  hole piece       group        id provincia
##   1 -2.368584 43.17667     1 FALSE     1 ALTO DEBA.1 ALTO DEBA  Gipuzcoa
##   2 -2.368524 43.17666     2 FALSE     1 ALTO DEBA.1 ALTO DEBA  Gipuzcoa
##   3 -2.368412 43.17666     3 FALSE     1 ALTO DEBA.1 ALTO DEBA  Gipuzcoa


euskadi5 +
  geom_polygon(data=prueba, aes(x=long, y=lat, group=group, fill=provincia), alpha=0.3, color="black") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero), color="red") +
  scale_size(range = c(4, 13)) +
  ggtitle("Clubes de la F.V.A.S.") +
  theme(plot.title = element_text(face="bold", size=32))

euskadi4 +
  geom_polygon(data=prueba, aes(x=long, y=lat, group=group, fill=provincia), alpha=0.3, color="black") +
  geom_point(data=clubes_2015, aes(x=lon, y=lat, size=Numero), color="red") +
  scale_size(range = c(4, 13)) +
  ggtitle("Clubes de la F.V.A.S.") +
  theme(plot.title = element_text(face="bold", size=32))


#====================================================================
# Pasos posteriores, 
#====================================================================


#====================================================================
# Referencias interesantes
#====================================================================


# Configurar títulos, leyendas...
#-----------------------------------------------------------------------------
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
# http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
# http://www.sharpsightlabs.com/format-titles-and-axes-in-ggplot2/



# Exportar las imágenes
#-----------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/Cairo/index.html
