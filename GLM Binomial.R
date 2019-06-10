#GLM BINOMIAL----

#Estos datos son de la supervivencia de los pasajeros del titanic
#Nos interesa saber la relación de la supervivencia de los pasajeros según la clase
titanic<- read.csv("~/Desktop/Tesis/R/Tutoriales y scripts/Scripts/LM-GLM-GLMM-Curso Paco/data-raw/Titanic_long.csv")
head(titanic)

#Vemos el total de pasajeros según la clase
total<-summary(titanic$class)

#Y ahora vemos los supervivientes
survivors<-summary(subset(titanic$class, subset = (titanic$survived == 1)))
#Y los no supervivientes
dead<-summary(subset(titanic$class, subset = (titanic$survived == 0)))

#Con barritas lo veremos mejor
barplot(summary(titanic$class), ylim = c(0,900), main = "Tripulantes del titanic")
barplot(survivors, ylim = c(0,900), main = "Supervivientes")
barplot(dead, ylim = c(0,900), main = "Muertos")

#Es obvio que algo raro ha pasado que los pobres y la tripulación se han muerto
#menos que la primera clase!!


#Veamos primero el modelo lineal
m5 <- lm(survived ~ class, data = titanic)
#Vemos que los residuos salen rarísimos, que no encajan con lo esperado 
#(la línea gris)
plot(m5)

#Let's see the distribution of the residuals
hist(m5$residuals)


#Residuals are not normal, and variance is not constant, we can't use lm,
#we use glm!

dead

#Vemos la porporción de supervivientes de cada clase
survivors/total

library(dplyr)

#Vemos la superviviencia de pasajeros según clase
#Hacemos que nos agrupe por primero clase y luego superviviencia
group_by(titanic, class, survived)
#vemos la agrupación del group_by con el summarise, que es específico pa esta función
summarise(group_by(titanic, class, survived), count =
            n())

plot(factor(survived) ~ class, data=titanic)

#Y ahora hacemos el glm, survived será la variable a explicar, según la clase
tit.glm<-glm(survived ~ class, data = titanic, family = binomial)
summary(tit.glm)

#These are the parameter estimates
coef(tit.glm)

# We need to back-transform: apply inverse logit
coef(tit.glm)


library(boot)

#The reverse logit of the intercept is the proportion of the crew who survived
inv.logit(coef(tit.glm)[1])
survivors/total

#We add the intercept (the baseline) to the parameter estimate, to match the survival
#of the first class
inv.logit(coef(tit.glm)[1] + coef(tit.glm)[2])
survivors/total

#The same for the second and third class
inv.logit(coef(tit.glm)[1] + coef(tit.glm)[3])
inv.logit(coef(tit.glm)[1] + coef(tit.glm)[4])
survivors/total

#Ahora pasamos a interpretar el modelo
install.packages("effects")
library(effects)

#All effects te dice los efectos de la clase en la supervivencia
allEffects(tit.glm)

#We see the effects plotted
plot(allEffects(tit.glm))

#And the residuals, which are not well adjusted
plot(tit.glm)

#Podemos diagnosticar los residuos con el paquete DHARMa
library(DHARMa)
simulateResiduals(tit.glm, plot = TRUE)

#Teóricamente ya tenemos el modelo hecho y comprobado y chequeado, ahora vamos a 
#jugar con él

#Tuvieron los hombres más superviviencia que las mujeres?
plot(factor(survived) ~ sex, data = titanic)

#Algo parece que hay, hagamos el modelo!
tit.sex <-glm(formula = survived ~ sex, data = titanic, family = binomial)
summary(tit.sex)

coef(tit.sex)

inv.logit(coef(tit.sex)[1])
inv.logit(coef(tit.sex)[1] + coef(tit.sex)[2])

allEffects(tit.sex)
plot(allEffects(tit.sex))

#What's behind women higher survival, was travelling in first class?

summary(titanic$class)
titanic$sex
fem.sup<-subset(titanic, subset = (titanic$sex == "female" & titanic$survived == 1))
fem.ded<-subset(titanic, subset = (titanic$sex == "female" & titanic$survived == 0))
mal.sup<-subset(titanic, subset = (titanic$sex == "male" & titanic$survived == 1))
mal.ded<-subset(titanic, subset = (titanic$sex == "male" & titanic$survived == 0))

summary(titanic$sex)
summary(fem.sup$class)
summary(mal.sup$class)
summary(fem.ded$class)
summary(mal.ded$class)


#La forma pro de ver esto es
tapply(titanic$survived, list(titanic$class, titanic$sex), sum)

#Lo que queremos ver es la interacción entre clase y sexo, no por clase, o por sexo
tit.sex.class <- glm(survived ~ class * sex, data = titanic, family = binomial)
summary(tit.sex.class)


#Ser mujer te daba más probabilidades de sobrevivir
allEffects(tit.sex.class)
plot(allEffects(tit.sex.class))

#LOGISTIC REGRESSION----
#Usamos los mismos datos pero esta vez en proporciones, no en lista
tit.prop <- read.csv("~/Desktop/Tesis/R/Tutoriales y scripts/Scripts/LM-GLM-GLMM-Curso Paco/data-raw/Titanic_prop.csv")
tit.prop
summary(tit.prop)

#Usamos tanto sobrevivir como no sobrevivir como variable respuesta, para ello
#usamos un cbind de ambas columnas
prop.glm <- glm(cbind(Yes, No) ~ Class, data = tit.prop, family = binomial)

#We checked that the results are the same, no matter what the form of the data is
#because is the same data
allEffects(prop.glm)
allEffects(tit.glm)

#Con ambos datos podemos obtener los mismos resultados!

#La regresión logística la podemos usar con predictores contínuos
#Cargamos estos datos que tienen la mortalidad infantil cada 1000 nacimientos y 
#el producto interior bruto

gdp<- read.csv("~/Desktop/Tesis/R/Tutoriales y scripts/Scripts/LM-GLM-GLMM-Curso Paco/data-raw/UN_GDP_infantmortality.csv")
#GDP significa producto interior bruto

plot(infant.mortality ~ gdp, data = gdp, main = "Infant mortality (per 1000 births)")

#Calculamos la tasa de superviviencia para cada 1000 nacimientos
survival<- (1000 - gdp$infant.mortality)
gdp$infant.survival <- survival

#Y hacemos el modelo binomial
gdp.glm<-glm(cbind(infant.mortality, infant.survival) ~ gdp, data = gdp, 
             family = binomial)

allEffects(gdp.glm)

#Veamoslo más claro en un gráfico

plot(allEffects(gdp.glm))

#Hacemos un gráfico con los datos y el modelo superpuesto
#(No entiendo muy bien la "x" esa)
plot(infant.mortality/1000 ~ gdp, data = gdp, main = "Infant mortality rate")
curve(inv.logit(coef(gdp.glm)[1] + coef(gdp.glm)[2]*x), from = 0, to = 40000, 
      add = TRUE, lwd=3, col="red")

#podemos plotear haciendo visreg, que nos plotea nuestro modelo
install.packages("visreg")
library(visreg)
visreg(gdp.glm, scale = "response")
#Y le metemos nuestras observaciones como puntos
points(infant.mortality/1000 ~ gdp, data = gdp)


#Diagnóstico de residuos con DHARMa
library(DHARMa)
simulateResiduals(gdp.glm, plot = TRUE)

#Estudiemos ahora la sobredispersión
simres <- simulateResiduals(gdp.glm, refit = TRUE)
testOverdispersion(simres)

#Overdispersion in logistic regression with proportion data
gdp.overdisp <- glm(cbind(infant.mortality, infant.survival) ~ gdp,
                    data = gdp, family = quasibinomial)


summary(gdp.overdisp)
plot(gdp.overdisp)
#Mean estimates do not change after accounting for overdispersion

allEffects(gdp.overdisp)
allEffects(gdp.glm)

#But standard errors (uncertainty) do!
par(mfrow = c(1,2))
visreg(gdp.overdisp, scale = "response")
points(infant.mortality/1000 ~ gdp, data = gdp)
visreg(gdp.glm, scale = "response")
points(infant.mortality/1000 ~ gdp, data = gdp)
par(mfrow = c(1,1))


#Whenever you fit logistic regression to proportion data, check family
#quasibinomial.
