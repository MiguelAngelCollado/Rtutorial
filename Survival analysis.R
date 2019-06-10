#How to do a survival analysis

library(survival)
dat <- read.delim("~/Desktop/Tesis/R/Tutoriales y scripts/Scripts/Survival Analysis/Innovation.txt")
View(dat)

str(dat)
attach(dat)
#Creamos una string con las IDs
ind  <- as.factor(ID_)
detach(dat)

#Queremos dibujar las curvas Kaplan meier (curvas de superviviencia)
#Draw Kaplan meier curves
head(dat)
#Surv crea un objeto de supervivencia
#survfit crea la curva de supervivencia

#Vemos la innovación y cuando se hizo el evento de la innovación para curvas
#según la población y el habitat
dat.surv <- survfit(Surv(time_e, status_e) ~ Population + Habitat2, na.action = na.exclude, data = dat) 

plot(dat.surv, lty = 1:4, xlab="Time to solve the task (msec)", ylab="% individuals that has no solved the task") 
legend(1000000, .5, c("New D", "New E", "Cam D", "Cam E"), lty = 1:4) 
title("Kaplan-Meier Curves\nfor Innovation") 

#Vemos la innovación y cuando se hizo el evento de la innovación para curvas 
#según el habitat
dat.surv1 <- survfit(Surv(time_e, status_e) ~ Habitat2, na.action = na.exclude, data = dat) 
plot(dat.surv1, lty = 1:2, xlab="Time to solve thetask (msec)", ylab="% individuals that has no solved the task") 
legend(1000000, .5, c("Downtown", "Edge"), lty = 1:2) 
title("Kaplan-Meier Curves\nfor Innovation") 

# podemos testar la diferencia en las curvas, son chi-cuadrados


test1  <- survdiff (Surv(time_e, status_e) ~ Population + Habitat2, na.action = na.exclude, data = dat)
test1

test2  <- survdiff (Surv(time_e, status_e) ~ Habitat2, na.action = na.exclude, data = dat) 
test2

test3  <- survdiff (Surv(time_e, status_e) ~ strata(Population) + Habitat2, na.action = na.exclude, data = dat)
test3 
test3$strata



#Comprobamos si el sexo, la cohorte o el objeto de color tiene alguna influencia,
#en la respuesta para las diferencias en un factor.

#Sexo
surv.sex <- survfit(Surv(time_e, status_e) ~ Sex_filled, na.action = na.exclude, data = dat) 
plot(surv.sex, lty = 1:2, xlab="Time to solve the task (msec)", ylab="% individuals that has no solved the task") 
legend(10000, .3, c("Female", "Male"), lty = 1:2) 

#Son diferentes
survdiff (Surv(time_e, status_e) ~ Sex_filled, na.action = na.exclude, data = dat)

#Esto ajusta un Cox proportional hazards regression model
#El modelo de Cox para datos censurados de superviviencia, especifica el peligro
#o la tasa de fallo para el tiempo de supervivencia de un individuo con un vector
#de covarianza que puede depender del tiempo.
# El efecto de la covariable se asume que multiplica el peligro de alguna forma 
# constante

#El coxph usa como resultado el likelihood ratio test, que se usa para comparar la 
#bondad de ajuste de dos modelos estadísticos, uno de los cuales (el modelo nulo)
#es un caso especial del otro (el modelo alternativo)

# The null hypothesis is that the smaller model is the “best” model; 
# It is rejected when the test statistic is large. In other words, 
# if the null hypothesis is rejected, then the larger model is a 
# significant improvement over the smaller one.

#Se interpreta igual como que son diferentes?
fit <- coxph(Surv(time_e, status_e) ~ Sex_filled, na.action = na.exclude, data = dat) 
fit

#Cohorte
surv.cohort <- survfit(Surv(time_e, status_e) ~ Cohort, na.action = na.exclude, data = dat) 
plot(surv.cohort, lty = 1:11, xlab="Time to solve the task (msec)", ylab="% individuals that has no solved the task") 

#Da diferencias
survdiff (Surv(time_e, status_e) ~ Cohort, na.action = na.exclude, data = dat) 

# Vemos si la motivación es diferente entre poblaciones o habitats

survdiff (Surv(time_motivation,status_motivation) ~ Population + Habitat2, na.action = na.exclude, data = dat)

survdiff (Surv(time_motivation,status_motivation) ~ Population, na.action = na.exclude, data = dat)

survdiff (Surv(time_motivation,status_motivation) ~ Habitat2, na.action = na.exclude, data = dat)


fit <- coxph(Surv(time_motivation, status_motivation)~ Population + Habitat2, na.action = na.exclude, data= dat) 
fit 
#They are all different


#Y si la motivación afecta a la respuesta
fit1 <- coxph(Surv(time_e, status_e) ~ time_motivation, na.action = na.exclude, data = dat) 
fit1

fit2 <- coxph(Surv(time_e, status_e) ~ strata(Origin)*time_motivation, na.action = na.exclude, data = dat) #motivation did not influence the resluts within each group
fit2

#Parece que si??



#Modelo construido para entender las chi-cuadrado
##We compare to exactly equal curves
lol<-c("AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO")
lol2<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
lol3<-c(10,100,200,300,400,500,800,1000,1500,2000,2200,2500,15,105,205,305,405,505,805,1005,1505,2005,2205,2500)
data.curves<-data.frame(lol,lol2,lol3)
test.surv.curves <- survfit(Surv(lol3, lol2) ~ lol, na.action = na.exclude, data = data.curves) 
plot(test.surv.curves, lty = 1:2, xlab="a test", ylab="this is", main="Comparing similar curves") 
legend(100, .2, c("Curve 1", "Curve 2"), lty = 1:2) 
survdiff (Surv(lol3, lol2) ~ lol, na.action = na.exclude, data = data.curves)

#And two known different curves
lol4<-c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)
lol5<-c(10,100,200,300,400,500,800,1000,1500,2000,2200,2500,2500,2000,2100,2500,2000,2500,2000,2500,2000,2100,2000,2300)
data.curves2<-data.frame(lol,lol4,lol5)
test2.surv.curves <- survfit(Surv(lol5, lol4) ~ lol, na.action = na.exclude, data = data.curves2) 
plot(test2.surv.curves, lty = 1:2, xlab="a test", ylab="this is", main= "Comparing different curves") 
legend(100, .2, c("Curve 1", "Curve 2"), lty = 1:2) 
survdiff (Surv(lol5, lol4) ~ lol, na.action = na.exclude, data = data.curves2)







#########################









#We attach a previosly done Pca.model
Pca.morph <- read.csv("~/Desktop/Tesis/R/Tutoriales y scripts/Scripts/Survival Analysis/pcamorph.txt", sep="")
dat<-cbind(dat, Pca.morph)

#previously attach morph from NF.R
fit3 <- coxph(Surv(time_e, status_e) ~ Pca.morph + Sex_filled, na.action = na.exclude, data = dat) 
fit3


# Fit the non-parametric cox model:

fit3 <- coxph(Surv(time_e, status_e)~ Population + Habitat2, na.action = na.exclude, data= dat) 
fit3 

fit4 <- coxph(Surv(time_e, status_e) ~ Population + Habitat2 + time_motivation, na.action = na.exclude, data = dat) 
fit4

fit4 <- coxph(Surv(time_e, status_e) ~ time_motivation, na.action = na.exclude, data = dat) 
fit4

fit4 <- coxph(Surv(time_e, status_e) ~ Pca.morph, na.action = na.exclude, data = dat) 
fit4

fit5 <- coxph(Surv(time_e, status_e)~ Habitat2 + Population + frailty (time_motivation),na.action = na.exclude, data= dat) 
fit5 

# Segundo, ajustamos Parametric Accelerated failure models
# Estos son un modelo alternativo a los Proportional Hazard Models,
# Aquí se asume que el efecto de la covariable acelera o decelera 
# el curso de la vida de una enfermedad por alguna constante
# Hay que especificarle una distribución
m1  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="exponential", na.action = na.exclude, data = dat)
summary(m1) 

m2  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="extreme", na.action = na.exclude, data = dat)
summary(m2) # Los que dan infinito no valen

m3  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="weibull", na.action = na.exclude, data = dat)
summary(m3)

m4  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="gaussian", na.action = na.exclude, data = dat)
summary(m4) # Los que dan infinito no valen

m5  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="logistic", na.action = na.exclude, data = dat)
summary(m5) # Los que dan infinito no valen

m6  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="lognormal", na.action = na.exclude, data = dat)
summary(m6) 

m7  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2, dist ="loglogistic", na.action = na.exclude, data = dat)
summary(m7) 

anova(m1,m3)# 0.02 (models are different... but wich one is best?)
anova(m1,m7)# 0.02 (models are different... but wich one is best?)
anova(m1,m6)# 0.02 (models are different... but wich one is best?)

#and including motivation:

m8  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2 + time_motivation, dist ="exponential", na.action = na.exclude, data = dat)
summary(m8)#Nan's produced!!!

m9  <- survreg(Surv(time_e, status_e) ~ Population + Habitat2 + frailty(time_motivation), dist ="exponential", na.action = na.exclude, data = dat)
m9 

# idem for visitation

#Draw Kaplan meier curves

dat.surv <- survfit(Surv(time_v, status_v) ~ Population + Habitat2, na.action = na.exclude, data = dat) 
plot(dat.surv, lty = 1:4) 
legend(1500000, .9, c("New D", "New E", "Cam A(D)", "Cam B(E)"), lty = 1:4) 
title("Kaplan-Meier Curves\nfor Innovation") 

dat.surv1 <- survfit(Surv(time_v, status_v) ~ Habitat2, na.action = na.exclude, data = dat) 
plot(dat.surv1, lty = 1:2) 
legend(1500000, .9, c("Downtown", "Edge"), lty = 1:2) 
title("Kaplan-Meier Curves\nfor Innovation") 

# test for differences in the curves

test1  <- survdiff (Surv(time_v, status_v) ~ Population + Habitat2, na.action = na.exclude, data = dat)
test1

test2  <- survdiff (Surv(time_v, status_v) ~ Habitat2, na.action = na.exclude, data = dat) #ns
test2

test3  <- survdiff (Surv(time_v, status_v) ~ strata(Population) + Habitat2, na.action = na.exclude, data = dat)
test3 # ns
test3$strata

# First we chack if Sex, Cohort and object color has any influence in Response, test for differences in one factor.

survdiff (Surv(time_v, status_v) ~ Sex, na.action = na.exclude, data = dat)

survdiff (Surv(time_v, status_v) ~ Sex_filled, na.action = na.exclude, data = dat)

#and if motivation affects the response

fit1 <- coxph(Surv(time_v, status_v) ~ time_motivation, na.action = na.exclude, data = dat) # motivation alone influence the resluts
fit1

fit2 <- coxph(Surv(time_v, status_v) ~ strata(Origin)*time_motivation, na.action = na.exclude, data = dat) #motivation did not influence the resluts within each group?¿
fit2

# Fit the non-parametric cox model:

fit3 <- coxph(Surv(time_v, status_v)~ Population + Habitat2, na.action = na.exclude, data= dat) 
fit3 

fit4 <- coxph(Surv(time_v, status_v) ~ Population + Habitat2 + time_motivation, na.action = na.exclude, data = dat) 
fit4

fit5 <- coxph(Surv(time_v, status_v)~ Habitat2 + Population + frailty(time_motivation),na.action = na.exclude, data= dat) 
fit5 

#Second we fit Parametric Accelerated failure models:

m1  <- survreg(Surv(time_v, status_v) ~ Population + Habitat2, dist ="exponential", na.action = na.exclude, data = dat)
summary(m1) 

m1  <- survreg(Surv(time_motivation, status_motivation) ~ Population + Habitat2, dist ="loglogistic", na.action = na.exclude, data = dat)
summary(m1) 

m3  <- survreg(Surv(time_v, status_v) ~ Population + Habitat2, dist ="weibull", na.action = na.exclude, data = dat)
summary(m3) 

m6  <- survreg(Surv(time_v, status_v) ~ Population + Habitat2, dist ="lognormal", na.action = na.exclude, data = dat)
summary(m6) 

m7  <- survreg(Surv(time_v, status_v) ~ Population + Habitat2, dist ="loglogistic", na.action = na.exclude, data = dat)
summary(m7) 

anova(m1,m3)# 0.02 (models are different... but wich one is best?)
anova(m1,m7)# 0.02 (models are different... but wich one is best?)
anova(m1,m6)# 0.02 (models are different... but wich one is best?)

#and including motivation:

m8  <- survreg(Surv(time_v, status_v) ~ Population + Habitat2 + time_motivation, dist ="exponential", na.action = na.exclude, data = dat)
summary(m8)#Nan's produced!!!

m9  <- survreg(Surv(time_v, status_v) ~ Population + Habitat2 + frailty(time_motivation), dist ="exponential", na.action = na.exclude, data = dat)
m9 

##### idem for bill exploration

#Draw Kaplan meier curves

dat.surv <- survfit(Surv(time_b, status_b) ~ Population + Habitat2, na.action = na.exclude, data = dat) 
plot(dat.surv, lty = 1:4) 
legend(1500000, .9, c("New D", "New E", "Cam A(D)", "Cam B(E)"), lty = 1:4) 
title("Kaplan-Meier Curves\nfor Innovation") 

dat.surv1 <- survfit(Surv(time_b, status_b) ~ Habitat2, na.action = na.exclude, data = dat) 
plot(dat.surv1, lty = 1:2) 
legend(1500000, .9, c("Downtown", "Edge"), lty = 1:2) 
title("Kaplan-Meier Curves\nfor Innovation") 

#and if motivation affects the response

fit1 <- coxph(Surv(time_b, status_b) ~ time_motivation, na.action = na.exclude, data = dat) # motivation alone influence the resluts
fit1

# Fit the non-parametric cox model:

fit3 <- coxph(Surv(time_b, status_b)~ Population + Habitat2, na.action = na.exclude, data= dat) 
fit3 

fit4 <- coxph(Surv(time_b, status_b) ~ Population + Habitat2 + time_motivation, na.action = na.exclude, data = dat) 
fit4

fit5 <- coxph(Surv(time_b, status_b)~ Habitat2 + Population + frailty(time_motivation),na.action = na.exclude, data= dat) 
fit5 

fit5 <- coxph(Surv(time_b, status_b)~ Habitat2 + Population + n_bill+ frailty(time_motivation),na.action = na.exclude, data= dat) 
fit5 

#Second we fit Parametric Accelerated failure models:

m1  <- survreg(Surv(time_b, status_b) ~ Population + Habitat2, dist ="exponential", na.action = na.exclude, data = dat)
summary(m1) 

m1  <- survreg(Surv(time_motivation, status_motivation) ~ Population + Habitat2, dist ="loglogistic", na.action = na.exclude, data = dat)
summary(m1) 

m3  <- survreg(Surv(time_b, status_b) ~ Population + Habitat2, dist ="weibull", na.action = na.exclude, data = dat)
summary(m3) 

m6  <- survreg(Surv(time_b, status_b) ~ Population + Habitat2, dist ="lognormal", na.action = na.exclude, data = dat)
summary(m6) 

m7  <- survreg(Surv(time_b, status_b) ~ Population + Habitat2, dist ="loglogistic", na.action = na.exclude, data = dat)
summary(m7) 

anova(m1,m3)# 0.02 (models are different... but wich one is best?)
anova(m1,m7)# 0.02 (models are different... but wich one is best?)
anova(m1,m6)# 0.02 (models are different... but wich one is best?)

#and including motivation:

m8  <- survreg(Surv(time_b, status_b) ~ Population + Habitat2 + time_motivation, dist ="exponential", na.action = na.exclude, data = dat)
summary(m8)#Nan's produced!!!

m9  <- survreg(Surv(time_b, status_b) ~ Population + Habitat2 + frailty(time_motivation), dist ="exponential", na.action = na.exclude, data = dat)
m9 



#finnally we test if time_v has an effect in time to e:

fit6 <- coxph(Surv(time_e, status_e)~ time_v,na.action = na.exclude, data= dat) 
fit6 

fit7 <- coxph(Surv(time_e, status_e)~ time_v + Habitat2 + Population,na.action = na.exclude, data= dat) 
fit7 

fit7a <- coxph(Surv(time_e, status_e)~ time_v + Habitat2 + Population + time_motivation,na.action = na.exclude, data= dat) 
fit7a

fit8 <- coxph(Surv(time_e, status_e)~ time_v + Habitat2 + Population + frailty(time_motivation),na.action = na.exclude, data= dat) 
fit8

fit9 <- coxph(Surv(time_e, status_e)~ strata(Origin)*time_v ,na.action = na.exclude, data= dat) 
fit9  

fit10 <- coxph(Surv(time_e, status_e)~ strata(Origin)*time_v + frailty(time_motivation),na.action = na.exclude, data= dat) 
fit10 

hist(duration_v) 
a  <- glm(duration_v ~ Population/Habitat2, family = poisson, na.action=na.exclude)
summary(a)

tapply(duration_v, Origin, mean)

hist(n_bill) 
a  <- glm(n_bill ~ Population/Habitat2, family = poisson, na.action=na.exclude)
summary(a)

tapply(n_bill, Origin, mean)

hist(status_e)
c <- glm(status_e ~ n_bill, family= binomial) 
summary(c)
plot(n_bill, status_e)

fit11 <- coxph(Surv(time_e, status_e)~ strata(Origin)*time_v + duration_v+ frailty(time_motivation),na.action = na.exclude, data= dat) 
fit11




#we analize status with a binomial error.
b  <- glm(status_motivation ~ Population/Habitat2, family = binomial, na.action=na.exclude)
summary(b)
b1  <- glm(status_e ~ Population/Habitat2, family = binomial, na.action=na.exclude)
summary(b1)
b2  <- glm(status_v ~ Population/Habitat2, family = binomial, na.action=na.exclude)
summary(b2)





##We compare to exactly equal curves
lol<-c("AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","AYY","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO","LMAO")
lol2<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
lol3<-c(10,100,200,300,400,500,800,1000,1500,2000,2200,2500,15,105,205,305,405,505,805,1005,1505,2005,2205,2500)
data.curves<-data.frame(lol,lol2,lol3)
test.surv.curves <- survfit(Surv(lol3, lol2) ~ lol, na.action = na.exclude, data = data.curves) 
plot(test.surv.curves, lty = 1:2, xlab="a test", ylab="this is", main="Comparing similar curves") 
legend(100, .2, c("Curve 1", "Curve 2"), lty = 1:2) 
survdiff (Surv(lol3, lol2) ~ lol, na.action = na.exclude, data = data.curves)

#And two known different curves
lol4<-c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)
lol5<-c(10,100,200,300,400,500,800,1000,1500,2000,2200,2500,2500,2000,2100,2500,2000,2500,2000,2500,2000,2100,2000,2300)
data.curves2<-data.frame(lol,lol4,lol5)
test2.surv.curves <- survfit(Surv(lol5, lol4) ~ lol, na.action = na.exclude, data = data.curves2) 
plot(test2.surv.curves, lty = 1:2, xlab="a test", ylab="this is", main= "Comparing different curves") 
legend(100, .2, c("Curve 1", "Curve 2"), lty = 1:2) 
survdiff (Surv(lol5, lol4) ~ lol, na.action = na.exclude, data = data.curves2)
