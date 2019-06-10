#Chisq test
#Típicos para comparar los efectos de tratamientos de medicamentos
df <- read.csv("https://goo.gl/j6lRXD")
table(df$treatment, df$improvement)

#Tomamos ambos vectores para hacer el chisq.test 
chisq.test(df$treatment, df$improvement, correct=FALSE)

#Como el p-value es <0.05, las variables son dependientes


#Como ejemplo extremo, podemos hacerlo con estos vectores, donde comparamos dos iguales
#y dos muy diferentes
a<-(c(3,4,5,3,4,5,3,4,5,3,4,5,3,4,5))
b<-(c(3,4,5,3,4,5,3,4,5,3,4,5,3,4,5))
b<-(c(5,6,6,10,14,15,13,14,15,13,4,5,3,4,5))
c<-(c(200,1,450,2,609,145,678,123,456,654,654,211,21,345,546))
chisq.test(a, b, correct=FALSE)
chisq.test(a, c, correct=FALSE)
x <- matrix(c(12, 5, 7, 7), ncol = 2)
x <- matrix(c(5, 6, 2, 5), ncol = 2)
ch <- chisq.test(x)
ch$observed
ch$expected

x <- matrix(c(15, 10, 5, 1), ncol = 2)
chisq.test(x)$p.value           # 0.4233

x <- matrix(c(10, 10, 1, 1), ncol = 2)
chisq.test(x)$p.value           # 0.4233

x <- matrix(c(5, 6, 2, 5), ncol = 2)
p <- matrix(c(5.5,5.5,3.5,3.5), ncol = 2)
ch <- chisq.test(x, rescale.p = p)           # 0.4233
ch$expected

x <- matrix(c(2, 6, 2, 5), ncol = 2)
p <- matrix(c(5.5,5.5,3.5,3.5), ncol = 2)
chisq.test(x, rescale.p = p)$p.value           # 0.4233
fisher.test(x)$null.value

x <- matrix(c(10, 5), ncol = 2)
chisq.test(x)$p.value           # 0.4233

#También podemos hacer el test exacto de fisher, que está diseñado para conteos
#en tablas de contingencia
fisher.test(a,b)
fisher.test(a,c)




####################Good example#
library(MASS)       # load the MASS package 
#This is a survey of frecuency of smokers (Smoke) and exercise (Exer)
survey
#We do a table with the count data
table(survey$Smoke, survey$Exer)
tbl<-table(survey$Smoke, survey$Exer) 
tbl 

#Is the exercise independent to smoking?
#Test the hypothesis whether the students smoking habit is independent 
#of their exercise level at .05 significance level.
chisq.test(tbl) 

#As the p-value 0.4828 is greater than the .05 significance level, 
#we do not reject the null hypothesis that the smoking habit is independent 
#of the exercise level of the students.

#In other words: smoking is independent of the exercise level!!

ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"])
colnames(ctbl)<-c("Smoking", "Little smoking")

chisq.test(ctbl)
#smoking is independent of the exercise level!! again


##CHI-SQUARED GOODNESS OF FIT

#same shit
observed = c(770, 230)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed,
           p = expected)

#Different shit
observed = c(500, 500)        # observed frequencies
expected = c(0.75, 0.25)      # expected proportions

chisq.test(x = observed,
           p = expected)
