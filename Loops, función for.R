#Loops
#Comando for
library(vegan)
for(i in 1:100){
  print("hello world")
  }
i


#Primero creamos un objeto con datos, para trabajar con él,
sqr = seq(1, 100, by= 2)
#Creamos un objeto vacío para rellenarlo, esto hay que crearlo antes del
#loop, porque a R no le gusta trabajar sobre cosas que no existen, 
#mantén siempre este comando fuera del loop y que no se sobrescriba
#durante el proceso
sqr.squared = NULL
#y aplicamos la función for, 
for (n in 1:50)
{
  sqr.squared[n] = sqr[n]^2
}
sqr.squared
#Le indicamos que lo haga 50 veces, porque sqr tiene 50 números y 
#queremos que el vector blanco lo rellene con el cuadrado de esos
#50 números, si le decimos menos, nos crea un vector más pequeño
#pero siguiendo dicha función, y si lo hacemos mayor, rellena con
#NAs hasta la cantidad pedida
sqr.squared = NULL
for (n in 1:25)
{
  sqr.squared[n] = sqr[n]^2
}

sqr.squared = NULL
for (n in 1:100)
{
  sqr.squared[n] = sqr[1]^2
}




#Funciones
cuadrado<-function(data){
  sqr.squared = NULL
  for (n in 1:length(data))
  {
    sqr.squared[n] = data[n]^2
  }
print(sqr.squared)
  }

cuadrado(sqr)
