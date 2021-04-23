# 03 - Modelos estadísticos

# para el primer ejemplo, veremos la relación entre la 
# profundidad y la magnitud de nuestros datos de terremotos
eq <- read.csv2("data/Earthquake_data.csv",
                header = TRUE,
                dec = ".",
                stringsAsFactors = FALSE)
head(eq)

# en la siguiente sesión aprenderemos a visualizar datos. 
# Por ahora, fijaos en el resultado de esto
library(tidyverse)
eq %>% ggplot(aes(x = Dep, y = M)) + 
  geom_point()

# ¿cómo funciona la función lm?
?lm

# nuestra primera regresión lineal! 
# ¿viene la magnitud de un terremoto explicada por su profundidad?
m1 <- lm(formula = M ~ Dep, data = eq)

# hemos guardado los resultados del modelo en una variable, m1. ¿Y ahora?
m1
# ¿qué tipo de objeto es m1?
str(m1)
# mucha información, pero la clave es la primera línea... 
# el resultado de llamar a la función lm es una lista!
# podemos obtener un resumen del modelo con la función "summary"
summary(m1)

coefs <- m1$coefficients
coefs

ggplot(eq, aes(x = Dep, y = M)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# información sobre personajes de Star Wars
personajes_SW <- read.csv2(file = "../data/starwars_info_personajes.csv",
                           header = TRUE,
                           stringsAsFactors = FALSE)
head(personajes_SW)

# tenemos altura (height) y peso (mass) de cada personaje
sw1 <- lm(formula = mass ~ height, data = personajes_SW)
summary(sw1)

ggplot(personajes_SW, aes(x = height, y = mass)) + 
  geom_point()

# ¿cuál es el peso máximo de los datos?
# literalmente: cuál es el nombre (personajes_SW$name) 
# de la fila cuyo peso (mass) es igual al peso máximo 
# de cualquiera de las filas (función "max").
peso.max <- which(personajes_SW$mass == max(personajes_SW$mass,na.rm = TRUE))
peso.max # el peso máximo es el del personaje de la fila 16
personajes_SW$name[peso.max] # ¿cuál es su nombre?

# eliminamos sólo la fila que corresponde al peso máximo
personajes_2 <- personajes_SW[-peso.max,] 

# y repetimos el modelo con los datos nuevos
sw2 <- lm(formula = mass ~ height, data = personajes_2)
summary(sw2)

# lo visualizamos
ggplot(personajes_2, aes(x = height, y = mass)) + 
  geom_point() + 
  geom_smooth(method = "lm")

sw3 <- lm(formula = mass ~ height + birth_year,data = personajes_2)
summary(sw3)
