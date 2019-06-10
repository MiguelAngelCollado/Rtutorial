#Regresión lineal
grasas <- read.table("http://www.uam.es/joser.berrendero/datos/EdadPesoGrasas.txt", 
                     header = TRUE)
names(grasas)
View(grasas)
pairs(grasas)
regresion <- lm(grasas ~ edad, data = grasas)
summary(regresion)
plot(grasas$edad, grasas$grasas, xlab = "Edad", ylab = "Grasas")
abline(regresion)
