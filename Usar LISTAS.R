#Usar listas
# Las listas te permiten contener diferentes tipos de objeto en uno solo, como estos tres

vec <- 1:4
df <- data.frame(y = c(1:3), x = c("m", "m", "f"))
char <- "Hello!"

#creamos nuestra lista con un vector, un data frame y un carácter
list1 <- list(vec, df, char)

# print list
list1

# Podemos también convertir objetos normales en listas, con la función as.list()
#En este caso cada elemento del vector se convierte en un componente diferente de la lista
as.list(vec)

#Podemos nombrar los componentes de una lista, útil para luego extraerlos
names(list1) <- c("Numbers", "Some.data", "Letters")
list1
#O nombrarlos directamente durante la creación de la lista
another.list <- list(Numbers = vec, Letters = char)
another.list

#Para extraer componentes de una lista

#Usamos [[ ]] que nos da el componente de la lista, con el que se puede operar
#Si usamos [ ] nos dará una "lista" de un componente 
list1[[3]]
list1[3]

list1[["Letters"]]
list1["Letters"]

#Subset a list
list1[c(1,3)]

#Dataframes dentro de listas, usamos [ ]
list1[[2]][1,]

#Formas de agregar nuevos elementos

#Con el $, podemos por ejemplo agregar un  modelo lineal
list1$newthing <- lm (y ~ x, data = df)
list1

list1[[5]] <- "New component"
list1

#Para eliminar componentes
list1$Letters <- NULL
list1

#Describir/explorar una lista
#class nos dice qué tipo de componentes tenemos en la list

class(list1)
class(list1[[1]])
class(list1[[2]])
class(list1[[3]])
class(list1[[4]])

#El número de componentes lo vemos con el tradicional length() 
length(int)

#Un sumario de la lista lo vemos con el tradicional str()
str(list1)

##Con el truquito de la length podríamos crear una lista infinita
new.list <- list(vec,char)
new.list[[length(new.list)+1]] <- "Infinito"

#Crear una lista vacía
list2 <- vector("list",3)

View(new.list)
unlist(new.list)
unlist(int)
