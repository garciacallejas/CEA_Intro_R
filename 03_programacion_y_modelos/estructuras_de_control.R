# 03 - Estructuras de control

# dataframe sencillo como ejemplo
d1 <- data.frame(caracter = c("a","b","c"), valor = c(1,2,4))

# si el caracter d es uno de los valores en la columna "caracter",
# sacamos un mensaje por pantalla. En caso contrario, sacamos otro mensaje.
if("d" %in% d1$caracter){
  cat("el caracter d está incluido en d1")
}else{
  cat("d no está en d1")
}

eq <- read.csv2(file = "../data/Earthquake_data.csv",
                dec = ".",
                header = TRUE,
                stringsAsFactors = FALSE)

# nombre de la columna que nos interesa
mi.columna <- "Dep"

# creamos una variable vacía para almacenar 
# las operaciones que haremos
resultado <- NA

# esta es una manera estándar de comprobar si una columna está en un dataframe
if(mi.columna %in% names(eq)){ 
  
  # si tenemos esta columna, podemos tener otra condición dentro de este código
  # esta condición pregunta literalmente 
  # "si la suma de NAs en la columna Dep es igual a cero"
  if(sum(is.na(eq[,mi.columna])) == 0){ 
    
    resultado <- mean(eq[,mi.columna])
    
  # si no se cumple la condición es que hay al menos un NA en la columna Dep
  }else{ 
    
    # ¿cuántos NA?
    num.nas <- sum(is.na(eq[,mi.columna])) 
    # y la condición contraria, ¿cuántos no son NA?
    datos.validos <- sum(!is.na(eq[,mi.columna])) 
    # lo sacamos por pantalla
    cat(paste("existen",
              num.nas,
              "NAs en los datos, la media se calculará con",
              datos.validos,
              "observaciones"))
    
    # el argumento na.rm elimina los NA antes de calcular la media
    resultado <- mean(eq[,mi.columna],na.rm = TRUE) 
  }# aquí acaba el segundo if-else
  
}else{ # si no se cumple la primera condición es que no existe la columna Dep
  cat(paste("no existe la columna",mi.columna,"en los datos leidos."))
}# aquí acaba el primer if-else

cat(paste("el resultado es",resultado))


for(i in 1:5){
  cat(paste("el contador tiene valor",i,"\n"))
}

for(contador in 1:10000){
  # repetimos 10000 veces un cierto código 
  # con una variable interna que se llama "contador".
}

# un vector numérico
vector.num <- c(2,6.7,333,80)

for(i in vector.num){
  cat(paste(i,"\n"))
}

# o un vector de caracteres
vector.char <- c("a","b","c")

for(i in vector.char){
  cat(paste(i,"\n"))
}



naves_SW <- read.csv2("../data/starwars_personajes_naves.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)
head(naves_SW)

# las funciones nrow() y length() nos devuelven el número de 
# filas y columnas de un dataframe, respectivamente
num.personajes <- nrow(naves_SW)

# este será mi vector de resultados. Un elemento por cada personaje, 
# que almacene el número de vehículos que conduce.
num.naves <- numeric(num.personajes)

# recordad que un vector también puede tener nombres asociados a sus elementos
names(num.naves) <- naves_SW$name

# el vector se inicia por defecto a cero
head(num.naves)

# para cada personaje, calculamos cuántas naves conduce 
# sumando los valores TRUE de su fila.
# y guardamos ese resultado en el vector "num.naves"
for(pers in 1:num.personajes){
  # puedo usar el contador "pers" 
  # para trabajar en la fila adecuada del dataframe.
  # a la vez, selecciono las columnas que me interesan
  # recordad: dataframe[fila(s),columna(s)]
  mis.naves <- naves_SW[pers,2:11]
  
  # usamos la función rowSums, porque la variable "mis.naves" 
  # es también un dataframe
  # fijaos que podemos usar nuestro contador "pers" para decir 
  # en qué posición del vector de resultados guardamos los datos.
  num.naves[pers] <- rowSums(mis.naves)
}

head(num.naves)
num.naves["Chewbacca"]

mi_vector <- 1:10
mi_vector

# esta operación se aplica de manera automática 
# a todos los elementos del vector
# esto es vectorización
resultado <- mi_vector^2

# si R no tuviera esta funcionalidad, 
# podríamos crear un bucle para hacer el mismo cálculo
resultado_for <- numeric(length(mi_vector))
# calcula el cuadrado de cada elemento y lo guarda en "resultado_for"
for(i in 1:length(mi_vector)){
  resultado_for[i] <- mi_vector[i]^2
}

resultado
resultado_for

mi.matriz <- matrix(data = c(1,2,3,4,5,6,7,8,9),nrow = 3)
mi.matriz

# el argumento MARGIN especifica si queremos aplicar 
# una función sobre cada fila (1) o sobre cada columna (2)
suma.filas <- apply(mi.matriz, MARGIN = 1, FUN = sum)
suma.columnas <- apply(mi.matriz, MARGIN = 2, FUN = sum)
suma.filas
suma.columnas

# otro ejemplo
media.filas <- apply(mi.matriz,MARGIN = 1, FUN = mean)
media.filas

# usamos de nuevo los datos de terremotos
eq <- read.csv2(file = "../data/Earthquake_data.csv",
                dec = ".",
                header = TRUE,
                stringsAsFactors = FALSE)

# queremos calcular la profundidad media y la magnitud media usando lapply
eq.clean <- eq[,c("Dep","M")]
head(eq.clean)

mean.values <- lapply(eq.clean, FUN = mean, na.rm = TRUE)
mean.values

# Fijáos que no hemos necesitado eliminar los NAs de los datos. 
# La función "mean" tiene un argumento na.rm 
# que permite eliminarlos automáticamente. 
# Este argumento (o cualquier otro que tenga la función que queramos usar) 
# también se puede pasar a lapply, como hemos visto.
# Si no le pasamos ese argumento extra, 
# la función "mean" fallará si encuentra NAs
mean.with.nas <- lapply(eq.clean, FUN = mean)
mean.with.nas

# definimos el vector donde almacenaremos los resultados
mean.with.for <- numeric(length(eq.clean))
# definimos un bucle que recorra todas las columnas (en este caso sólo dos)
for(i in 1:length(eq.clean)){
  mean.with.for[i] <- mean(eq.clean[,i],na.rm = TRUE)
}

# lapply devuelve una lista, aquí hemos definido un vector
mean.with.for
