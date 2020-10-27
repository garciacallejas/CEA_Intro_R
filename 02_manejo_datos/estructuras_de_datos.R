# 02 - Estructuras de datos

# una lista con dos elementos numéricos, un carácter, 
# y un vector de caracteres
mi.info <- list(18,04, "David", c("extremoduro","radio tarifa","tool"))
mi.info

# un solo corchete, como los vectores
l1 <- mi.info[1]
l1
# ¿qué tipo de dato devuelve?
str(l1)

# doble corchete
l2 <- mi.info[[1]]
l2
# ¿qué tipo de dato devuelve?
str(l2)

# un vector de caracteres
musica1 <- mi.info[4]
# nos devuelve una lista con un único elemento, que es nuestro vector
musica1
str(musica1)

musica2 <- mi.info[[4]]
# nos devuelve el vector de caracteres
str(musica2)

mi.info <- list(dia = 18, mes = 4, nombre = "David", 
                musica = c("extremoduro", "radio tarifa", "tool"))
mi.info

mi.musica <- mi.info[["musica"]]
mi.musica

# añado un quinto elemento
mi.info[[5]] <- c("breaking bad", "treme", "the expanse")
mi.info

# puedo nombrar a este quinto elemento
names(mi.info)[5] <- "series"
mi.info

# si quiero un número como nombre de un elemento, lo pongo entre comillas
mis.viajes <- list("2020" = c("Grazalema", "Parc del Cadí", "Cerdeña"), 
                   "2019" = c("Portugal", "Doñana", "Picos de Europa"))
mis.viajes

# añado la lista "mis.viajes" a la lista general
mi.info[[6]] <- mis.viajes
names(mi.info)[6] <- "viajes"
mi.info

# ¿y para acceder a los elementos de una lista dentro de otra lista?
viajes.20 <- mi.info[[6]][["2020"]]
viajes.20


# seguramente el dataframe más usado en todos los tutoriales de R... 
# características de diferentes especies de "Iris"
head(iris)

# ¿cómo crear un dataframe? fácil!
info.personal <- data.frame(dia = c(18,17), mes = c(4,5), nombre = c("David","Adam"))
info.personal

# para explorar un dataframe, tenemos varias funciones a nuestra disposición
# las primeras 6 filas
head(info.personal)
# tipos de datos en cada columna
str(info.personal)
# resumen de los datos
summary(info.personal)
# número de filas
nrow(info.personal)
# número de columnas
length(info.personal)
# nombres de las columnas
names(info.personal)

# ¿qué significa la coma?
info.personal[,"dia"]

info.personal$dia

# esto nos permite acceder cualquier combinación de fila y columna
info.personal$dia[1] # el día asociado a la primera observación
info.personal$nombre[2] # el nombre de la segunda observación


# no se puede añadir elemento a elemento, 
# porque no existe la fila 3 del dataframe
# info.personal$dia[3] <- 5

# lo ideal es crear un nuevo dataframe y añadirlo completo 
# con la función "rbind" (row-wise bind, literalmente enganche de filas). 
# Es como apilar piezas de Lego
nueva.fila <- data.frame(dia = 1, mes = 5, nombre = "Sofia")
info.personal <- rbind(info.personal,nueva.fila)

# La manera óptima de trabajar con dataframes es crear 
# desde cero la estructura, con todas las filas que necesitemos. 
# Esto permite a R asignar la memoria necesaria para esos datos, 
# aunque estén temporalmente "vacíos".

personas <- 1000

info.long <- data.frame(dia = numeric(personas), 
                        mes = numeric(personas), 
                        nombre = character(personas),
                        stringsAsFactors = FALSE)

# este dataframe contiene 1000 entradas, todas vacías
head(info.long)

# podemos "rellenarlo" columna a columna, 
info.long$mes <- 9

# o elemento a elemento
info.long$dia[2] <- 18
info.long$nombre[1] <- "Anna"

# pero hacerlo fila a fila es problemático
# info.long[3,] <- c(1,2,"Mario")

# fijáos que lo que estamos asignando es un vector de caracteres. 
# R asume que si mezclas números y caracteres en un mismo vector, 
# todo será convertido a carácter.
mi.vector <- c(1,2,"Mario")
str(mi.vector)

# Si asignamos esto a una fila de nuestro dataframe, 
# convertirá las columnas numéricas en columnas de caracteres.
# Y encima, lo hace sin avisar, así que tenemos que tener mucho cuidado!
info.long.copia <- info.long

info.long.copia[3,] <- c(1,2,"Mario")
str(info.long)
str(info.long.copia)

info.long$nueva.columna <- info.long$dia * info.long$mes

info.long$nueva.columna <- NULL
# info.long[2,] <- NULL

mi.matriz <- matrix(data = c(1,2,3,4,5,6,7,8,9),nrow = 3)
mi.matriz

# para acceder y modificar los elementos de una matriz, usamos los corchetes

# la primera fila
mi.matriz[1,]
# la primera columna
mi.matriz[,1]
# elemento de la segunda fila, tercera columna
mi.matriz[2,3]

# las filas y columnas se pueden nombrar
rownames(mi.matriz) <- c("fila1","fila2","fila3")
colnames(mi.matriz) <- c("col1","col2","col3")
mi.matriz

# al igual que en listas y dataframes, si tenemos una matriz nombrada, 
# podemos acceder a sus elementos por sus nombres
mi.matriz["fila1","col3"]



# suma
mi.matriz + mi.matriz

# multiplicación de matrices
mi.matriz %*% mi.matriz

# multiplicación elemento a elemento
mi.matriz * mi.matriz

# o por un escalar
mi.matriz * 2

# podemos extraer la diagonal
diag(mi.matriz)

# o la transpuesta
t(mi.matriz)


# de matriz a dataframe
mi.df <- as.data.frame(mi.matriz)
mi.df

# de dataframe a matriz
mi.matriz.2 <- as.matrix(info.long)
# hemos intentado transformar a matriz un dataframe que 
# contenía números y caracteres. R lo convierte en una matriz de caracteres
head(mi.matriz.2)

?array
