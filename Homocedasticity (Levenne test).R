#Cómo hacer test de Levene
require(car)

set.seed(10)
#Creamos un data frame con datos normales
#Primero los datos numéricos
x1 <- rnorm(30,10,1)
x2 <- rnorm(30,10,1.1)
x3 <- rnorm(30,10,1.2)

#Creamos ahora las categorías
f <- factor(c(rep("x1",30),rep("x2",30),rep("x3",30)))
x <- c(x1,x2,x3)

cbind(x,f)
var <- leveneTest(x ~ f)
#Las varianzas son iguales!
var
