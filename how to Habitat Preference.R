#In this code we are going to learn how to extract habitat preference using null models

#We download this package, very useful for bipartite networks, which includes null models 
#constructions
library(bipartite)

#First we create an example table, it must be an interaction matrix crossing species an habitats

#We create some fictional habitats with abundance
Meadow<-c(3,89,17,12,50,0,47,38)
Garden<-c(11,77,20,3,53,2,63,30)
Cave<-c(89,1,18,97,70,50,72,50)

#And add some fictional species
red<-data.frame(Meadow,Garden,Cave, row.names = c("Zubat","Caterpie","Clefairy","Geodude","Rattata","Haunter","Pidgey","Miltank"))

#This is our interaction matrix, it's important that our matrix has the same sampling effort
#for every habitat, if we don't have it, we must correct the data to simulate this
red


#Now we create some null models from our matrix, using the nullmodel function, and 
#using method = "rd2table" that generates random 2-way tables maintaining the sum of rows 
#and columns using Patefield's algorithm so the proportional abundance of species and habitats
#is maintained.

n.mod<-nullmodel(red, N=1000, method="r2dtable")

#And now we need to extract the quantile .95 and .05 from that nullmodels and to stablish
#threshold for preference/avoidance

#We need to do it for every habitat, first, Meadows
Meadow.05=NULL
Meadow.95=NULL
l=NULL

for(k in 1:(nrow(red))){
  for(n in 1:1000){
    (n.mod[[n]])[k,1]
    l[n]=(n.mod[[n]])[k,1]
  }
  l
  Meadow.05[k]=quantile(l, c(.05))
  Meadow.95[k]=quantile(l, c(.95))
}
#Second, gardens
Garden.05=NULL
Garden.95=NULL
l=NULL

for(k in 1:(nrow(red))){
  for(n in 1:1000){
    (n.mod[[n]])[k,2]
    l[n]=(n.mod[[n]])[k,2]
  }
  l
  Garden.05[k]=quantile(l, c(.05))
  Garden.95[k]=quantile(l, c(.95))
}
#Third, caves
Caves.05=NULL
Caves.95=NULL
l=NULL

for(k in 1:(nrow(red))){
  for(n in 1:1000){
    (n.mod[[n]])[k,3]
    l[n]=(n.mod[[n]])[k,3]
  }
  l
  Caves.05[k]=quantile(l, c(.05))
  Caves.95[k]=quantile(l, c(.95))
}

#Now we create preference and avoidance matrix
preference.red<-cbind(Meadow.95,Garden.95,Caves.95)
row.names(preference.red)<-row.names(red)
preference.red

avoidance.red<-cbind(Meadow.05,Garden.05,Caves.05)
row.names(avoidance.red)<-row.names(red)
avoidance.red
#And compare with our data

#Here, TRUE means preference for certain habitat
#Abundance higher than expected means preference
red>preference.red

#And here, TRUE means avoidance for that habitat
#And abundance lower than expected means avoidance
red<avoidance.red

#The rest of the species, would have indifference