#First we install the libraries we need
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
# and their dependencies

#Conseguir un mapa de un pa??s
library(dismo)
mapa <- gmap("United States")
plot(mapa)

#Podemos elegir qu?? tipo de mapa (roadmap, satellite, hybrid, terrain)
mapa2 <- gmap("United States", type = "hybrid")
plot(mapa2)

#Podemos elegir cantidad de zoom (el n??mero en exp indica el multiplicador de zoom)
mapa3 <- gmap("United States", type = "hybrid", exp=0.1)
plot(mapa3)

#Guardamos el mapa en nuestro working directory para su futuro uso
minnesota <- gmap("United States", type = "hybrid", exp=0.1)


#Seleccionamos una regi??n para ampliar
mapa4 <- gmap("United States", type = "hybrid")
plot(mapa4)
select.area <- drawExtent() #Aqu?? tenemos que seleccionar, clickando en el mapa
#los m??rgenes de nuestro mapa
mapa41<- gmap(select.area)
plot(mapa41)

?gmap #para ver otras opciones

#Podemos crear un mapa, que se nos ir?? a nuestro working directory, dando el centro
#de las coordenadas de la zona que queremos mapear, con el argumento `center`
library(RgoogleMaps)
prypiat<- GetMap(center = c(51.5, 24.1), zoom=10, 
                maptype = "satellite")
####################################################################################
#Este comando es para poner puntos en un mapa, pero pide el argumento "size" y no va
############################################################################muy bien
PlotOnStaticMap(maapa, 
                lat = c(36.3, 35.8, 36.4), lon = c(-5.5, -5.6, -5.8), 
                destfile=maapa, zoom = 10, 
                cex = 4, pch = 19, col = "red", FUN = points, add = F)
####################################################################################



#Para abrir mapas en un navegador
library(googleVis)

data(Exports)    # a simple data frame
Geo <- gvisGeoMap(Exports, locationvar="Country", numvar="Profit", 
                  options=list(height=400, dataMode='regions'))
plot(Geo)

##########################################################################
##DATOS ESPACIALES, para incluir en un mapa puntos, lineas y poligonos##
##########################################################################
#cargamos las librerias
library(dismo)
library(jsonlite)

#Cargamos conjunto de datos
laurus <- gbif("Laurus", "nobilis")
#Hacemos subset solo con las variables que nos interesan
locs <- subset(laurus, select = c("country", "lat", "lon"))
#Comprobamos que tenemos los datos que queremos
head(locs) 
#Descartamos los datos con errores en las coordenadas
locs <- subset(locs, locs$lat < 90)
#Comprobamos que no haya NA ni datos extra??os
View(locs)
#Tenemos un dataframe con coordinadas, podemos plasmarlas ahora espacialmente
coordinates(locs) <- c("lon", "lat")  
plot(locs)

#Definimos la proyecci??n geogr??fica, hay que ponerle un PROJ4 espec??fico a cada zona
#o algo as?? nuse nuse Xdxdxdzd
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)


#Cambiamos el color y tama??o de los puntos
plot(locs, pch = 20, col = "steelblue")
#Cargamos library que contiene muchos tipos de mapas, por ejemplo estos
library(rworldmap)
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

#Las instrucciones del paquete rworldmap en un formato bonico#
vignette("rworldmap")

#Vamos a hacer ahora un subset para mapear guay#

#Con este comando, vemos las localidades de la especie seg??n el pa??s
table(locs$country)

#Hagamos por ejemplo, un mapa para las especies en Reino Unido
#Primero creamos un subset con solo las especies en Reino Unido
locs.gb <- subset(locs, locs$country == "United Kingdom")
View(locs.gb)
#Hacemos el plot ahora
plot(locs.gb, pch = 20, cex = 2, col = "steelblue")
title("Laurus nobilis en Reino Unido")
plot(countriesLow, add = T)
plot(countriesLow)


#Probamos lo mismo con australia
locs.aus <- subset(locs, locs$country == "Australia")
View(locs.aus)
plot(locs.aus, pch = 20, cex = 2, col = "red")
title("Australia tkm")
plot(countriesLow, add=T)

##Para hacerlo m??s bonito con mapas del googlemaps##

gbmap <- gmap(locs.gb, type = "satellite")
locs.gb.merc <- Mercator(locs.gb)
plot(gbmap)
points(locs.gb.merc, pch = 20, col = "red")


ausmap <- gmap(locs.aus, type = "hybrid")
locs.aus.merc <- Mercator(locs.aus)
plot(ausmap)
points(locs.aus.merc, pch = 20, col = "red")


