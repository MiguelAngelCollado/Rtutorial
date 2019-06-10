library(fitdistrplus)
library(logspline)

#Ver distribución de tus datos
normal<-rnorm(100, mean = 5, sd = 3)
hist(normal)
descdist(normal, discrete = FALSE)

logistic<-rlogis(100, location = 0, scale = 1)
hist(rlogis(100, location = 0, scale = 1))
descdist(logistic, discrete = FALSE)
