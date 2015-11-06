# ==========================================================================
#                           Mapa con zoom
# ==========================================================================


# Descripción
# ==========================================================================

# A partir de una tabla de datos, que debe contener al menos las coordenadas
# geográficas y algún tipo de variable (téxto, número...), se genera un mapa 
# puede ser navegado, incluyendo funciones zoom.

# información sobre la función gvisMap en
# http://www.inside-r.org/packages/cran/googleVis/docs/gvisMap



# Cargar la librería
# ==========================================================================

library(googleVis)



# Pegar unos datos desde el portapapeles (para el ejemplo)
# ==========================================================================

# Por ejemplo, copiar estos datos
#oooooooooooooooooooooooo
tipo;lon;lat
A;-2.4637136;36.83405
A;-6.2885962;36.52706
A;-4.7793835;37.88818
B;-3.5985571;37.17734
B;-6.9447224;37.26142
B;-3.7849057;37.77959
C;-4.4212655;36.72126
A;-5.8930500;37.42017
B;-0.4078058;42.13184
C;-1.1064345;40.34569
A;-0.8890853;41.64882
C;-5.8593267;43.36140
#ooooooooooooooooooooooooo


datos <- read.delim("clipboard", sep=";", header=TRUE)

head(datos)
##   tipo       lon      lat
## 1    A -2.463714 36.83405
## 2    A -6.288596 36.52706
## 3    A -4.779383 37.88818
## 4    B -3.598557 37.17734
## 5    B -6.944722 37.26142
## 6    B -3.784906 37.77959



# Se crea una nueva variable, uniendo las coordenadas
# ==========================================================================

datos$latlon<-paste(datos$lat, datos$lon, sep=":")

str(datos)
## data.frame':  12 obs. of  4 variables:
## $ tipo  : Factor w/ 3 levels "A","B","C": 1 1 1 2 2 2 3 1 2 3 ...
## $ lon   : num  -2.46 -6.29 -4.78 -3.6 -6.94 ...
## $ lat   : num  36.8 36.5 37.9 37.2 37.3 ...
## $ latlon: chr  "36.83405:-2.4637136" "36.52706:-6.2885962" ...



# Se genera el mapa
# ==========================================================================

mapa<-gvisMap(datos, locationvar="latlon", tipvar="tipo", 
                    options = list(showTip=T, showLine=F, enableScrollWheel=TRUE,
                    useMapTypeControl=T, width=1400, height=800))



# Vemos el mapa. Se abre en una ventana del explorador por defecto del PC
# ==========================================================================

plot(mapa)

# para obtener el código html y poderlo incrustar en una web:

print(mapa)


################################ FIN #######################################
