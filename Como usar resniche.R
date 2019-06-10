#Como usar el extinto package "resniche"

library(indicspecies)
data(pigeons)

#Estos datos tienen tres objetos, relacionado con la preferencia de dietas de un par
#de poblaciones en barcelona y moia
diet.barcelona
diet.moia
#y su matriz de disimilaridad
dfood

#Hacemos un dendograma para verlo mejor
plot(hclust(dfood, method = "average"), h=-1, xlab="(0^0/)", ylab="Distance", main="Dendograma",sub="",ylim=c(0.2,1))



names(diet.barcelona)
names(diet.moia)
#Los recursos usados por el muestreo (sumando lo usado por cada individuo)
diet.pop.barcelona <- colSums(diet.barcelona)
diet.pop.moia <-colSums(diet.moia)
#Los expresamos como proporción
round(diet.pop.barcelona/sum(diet.pop.barcelona), digits = 3)

diet.pop.moia <- colSums(diet.barcelona)
round(diet.pop.moia/sum(diet.pop.moia), digits = 3)

#extraemos las métricas de nicho
nichevar(diet.barcelona, mode = "single")
nichevar(diet.moia, mode = "single")





int.mat.t <- t(int.mat)
row.names(int.mat.t)<-c("Barren Land","Coastal","Cultivated Crops","Deciduous Forest","Developed, High Intensity", "Developed, Low Intensity","Developed, Medium Intensity","Developed, Open Space", "Emergent Herbaceuous Wetlands","Evergreen Forest","Herbaceuous/Hay/Pasture","Mixed Forest","Shrub/Scrub","Woody Wetlands")


#Ejemplo de dendograma
require(graphics)
x <- matrix(rnorm(100), nrow = 5)
dist(x)
x[c(2,3,5),]
plot(hclust(dist(x), method = "average"))

d.int.mat.t <- dist(int.mat.t)
d2.int.mat.t <- dist(int.mat.t)

d.int.mat.t + d2.int.mat.t

str(hclust(dist(x), method = "average"))
#"single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
plot(hclust(dist(int.mat.t), method = "average"))
plot(hclust(dist(int.mat.t), method = "ward.D"))
plot(hclust(dist(int.mat.t), method = "ward.D2"))
plot(hclust(dist(int.mat.t), method = "single"))
plot(hclust(dist(int.mat.t), method = "complete"))
plot(hclust(dist(int.mat.t), method = "mcquitty"))
plot(hclust(dist(int.mat.t), method = "median"))
plot(hclust(dist(int.mat.t), method = "centroid"))






dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
d <- as.dist(m)
stopifnot(d == dist(x))

