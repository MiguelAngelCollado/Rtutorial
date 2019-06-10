# Curso Raster online http://neondataskills.org/R/Raster-Data-In-R/
library(raster)
library(rgdal)
library(sp)
getwd()
setwd()
#Cargamos la capita raster
DEM <- raster("/Users/Bartomeus_lab/Desktop/Tesis/R/Tutoriales y scripts/Scripts/CHM_InSitu_Data/DigitalTerrainModel/SJER2013_DTM.tif")
DEM

#Por defecto no tienen establecidos los valores mínimos y máximos
DEM <- setMinMax(x = DEM)

#Get min and max cell values from raster

#NOTE: this code may fail if the raster is too large
cellStats(DEM, min)
cellStats(DEM, max)
cellStats(DEM, range)

#Coordinate reference system
DEM@crs
#view raster extent
DEM@extent

DEM@class

plot(DEM, main="Killo")

#we can look at the distribution of values in the raster too
hist(DEM, main="Distribution of elevation values", 
     col= "purple", 
     maxpixels=21752940)

#Calculos básicos
#multiple each pixel in the raster by 2
DEM2 <- DEM * 2

DEM2

#plot the new DEM
plot(DEM2, main="DEM with all values doubled")

#También se pueden crear plots con esta función
image(DEM)

#specify the range of values that you want to plot in the DEM
#just plot pixels between 250 and 300 m in elevation
image(DEM, zlim=c(250,300))

#Colores----
#we can specify the colors too
col <- terrain.colors(5)
image(DEM, zlim=c(250,300), main="Digital Elevation Model (DEM)", col=col)
#add a color map with 5 colors
col=terrain.colors(5)
#add breaks to the colormap (6 breaks = 5 segments)
brk <- c(250, 300, 350, 400,450,500)
plot(DEM, col=col, breaks=brk, main="DEM with more breaks")

# Mover la gráfica y darle una leyenda
par(xpd = FALSE,mar=c(5.1, 4.1, 4.1, 4.5))
#DEM with a custom legend
plot(DEM, col=col, breaks=brk, main="DEM with a Custom (buf flipped) Legend",legend = FALSE)
#xpd back on to force the legend to fit next to the plot.
par(xpd = TRUE)
#add a legend - but make it appear outside of the plot
legend( par()$usr[2], 4110600,
        legend = c("lowest", "a bit higher", "middle ground", "higher yet", "Highest"), 
        fill = col)



#Croppig rasters-----
#plot the DEM
plot(DEM)
#Define the extent of the crop by clicking on the plot
cropbox1 <- drawExtent()

plot(cropbox1)
#crop the raster, then plot the new cropped raster
DEMcrop1 <- crop(DEM, cropbox1)

#plot the cropped extent
plot(DEMcrop1) #You can also manually assign the extent coordinates to be used to crop a raster.  We'll need the extent defined as (`xmin`, `xmax`, `ymin` , `ymax`) to do this.  This is how we'd crop using a GIS shapefile (with a rectangular shape)


#define the crop extent
cropbox2 <-c(255077.3,257158.6,4109614,4110934)
#crop the raster
DEMcrop2 <- crop(DEM, cropbox2)
#plot cropped DEM
plot(DEMcrop2)


HABS<- raster("/Users/Bartomeus_lab/Desktop/Tesis/R/habpref full data/NLCD_data/nlcd_2001_landcover_2011_edition_2014_10_10.img")
HABS@data@attributes

plot(HABS)
satellite.map(sd.7)
plot(HABS)

cropbox1 <- drawExtent()

#crop the raster
HABSc <- crop(HABS, cropbox1)
lecrop <- c(1077435,1771065,1600845,2141445)
randomcrop <- c(2077435,4071065,1600845,2141445)
HABS1 <- crop(HABS, lecrop)
plot(HABS1)
HABS@data@attributes
area(HABSc)
HABS@
levels(HABS)
levels(HABS1)
HABS2 <- setExtent(HABS, lecrop, keepres=TRUE, snap=FALSE)
HABS3 <- setExtent(HABS, randomcrop, keepres=TRUE, snap=TRUE)
levels(HABS3)
levels(HABS)
plot(HABS2)
mask
extent(HABS2)
extent(HABS1)
