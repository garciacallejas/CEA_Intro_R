knitr::opts_chunk$set(echo = TRUE, results = FALSE, warning = FALSE, message = FALSE)

# este paquete incluye ggplot2
library(tidyverse)

# leemos los datos de personajes de Star Wars
personajes_SW <- read.csv2(file = "../data/starwars_info_personajes.csv",
                           stringsAsFactors = FALSE)

# queremos crear una figura que muestre la altura y el peso de cada personaje, 
# donde cada personaje sea un punto

# lo primero que tenemos que entender es que un "plot", una figura, 
# también la podemos guardar en una variable.
# Para crear una figura nueva, usamos la función ggplot
# en esta función, tenemos que especificar qué datos 
# queremos usar para la figura

figura1 <- ggplot(data = personajes_SW)

# ¿qué contenido tiene este objeto? 
figura1 # en Rstudio, aparecerá la zona de "plots" en blanco.
class(figura1) # este objeto es de clase ggplot

# a partir de aquí, podemos ir añadiendo "capas" de información 
# a nuestra figura vacía
# lo primero que debemos pensar en incluir son los ejes de nuestra figura.
# Esto se puede hacer también en la función ggplot

# el argumento "aes" significa "aesthetics", es decir, 
# la estética, los detalles de la figura.
figura1 <- ggplot(data = personajes_SW,aes(x = height, y = mass))
figura1

# ahora deberíamos ver los ejes sobre los que queremos dibujar nuestros puntos.
# Estos puntos, uno por cada personaje, serán una capa nueva de información.
# Las capas se añaden en ggplot2 con las funciones "geom_", 
# literalmente añadiendo elementos al objeto que hemos creado
figura1 <- figura1 + geom_point()
figura1

## # ggsave por defecto usa pulgadas como unidades...
## # yo suelo ir haciendo ensayo-error con diferentes tamaños
## # hasta encontrar el balance adecuado entre tamaño de letra/puntos, y fondo
## ggsave(filename = "mi_figura.png",plot = figura1,width = 5,height = 5)

# lo primero que haremos es, como antes, eliminar a Jabba, 
# que nos descuadra toda la imagen.
sw2 <- subset(personajes_SW, name != "Jabba Desilijic Tiure")

# y creamos una figura en la que cada especie tenga un color diferente
# para esto añadimos un "aesthetic" más, 
# pero dentro de la función que dibuja los puntos.
figura2 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(aes(color = species))
  
# la figura funciona, pero hay muchas, 
# demasiadas especies como para que sea efectiva.
# podemos agrupar las especies en grupos más grandes, para simplificar. 
# ¿Cuántos personajes hay de cada especie?
table(sw2$species)

# de casi todas las especies hay un solo individuo. 
# Dejaremos los droides, los humanos,
# y, por curiosidad, a Yoda. El resto los agruparemos en una categoría "otros"
# primero, creamos una columna idéntica a "species"
sw2$species_group <- sw2$species
# y agrupamos aquellas que no son ni "Human", ni "Droid", ni "Yoda's species"
sw2$species_group[which(!sw2$species %in% c("Human",
                                            "Droid",
                                            "Yoda's species"))] <- "Other"
table(sw2$species_group)

# rehacemos nuestra figura con la nueva clasificación
figura2 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(aes(color = species_group))

# también podemos asignar valores fijos a todos los puntos. 
# Fijaos en la diferencia entre la orden anterior y esta
figura2.2 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(color = "darkgreen")


figura2.3 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(color = "darkgreen", size = 3, aes(shape = species_group))

# gráfico de barras: cuántos individuos hay por cada grupo de especies.
# Para esta figura no necesitamos añadir eje y, 
# porque este será el conteo de cada grupo.
# el conteo se calcula con geom_bar, añadiendo el argumento "stat = "count""
figura3 <- ggplot(sw2, aes(x = species_group)) +
  geom_bar(stat="count")

# como antes, podemos colorear cada grupo. 
# Para colorear areas, usamos la orden "fill",
# que significa, literalmente, "relleno".
figura3 <- ggplot(sw2, aes(x = species_group)) +
  geom_bar(stat="count", aes(fill = species_group))

# gráfico de cajas (boxplot)
figura4 <- ggplot(sw2, aes(x = species_group, y = height)) + 
  geom_boxplot(aes(fill = species_group))

# distribución de puntos 
figura5 <- ggplot(sw2, aes(x = species_group, y = height)) + 
  geom_jitter(aes(color = species_group))

# puntos y cajas
figura6 <- ggplot(sw2, aes(x = species_group, y = height)) + 
  geom_jitter(aes(color = species_group), alpha = .8) +
  geom_boxplot(aes(fill = species_group), alpha = .5) 

# histogramas
figura7 <- ggplot(sw2, aes(x = height)) + 
  geom_histogram(bins = 30)

figura8 <- ggplot(sw2, aes(x = height)) + 
  geom_density()



# Por defecto usa un método de regresión polinomial (loess)
figura9 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(aes(color = species_group)) + 
  geom_smooth()

# pero podemos especificar otros modelos
figura10 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(aes(color = species_group)) + 
  geom_smooth(method = "lm")

# igual que dividir el color por grupos, podemos dibujar
# una regresión por cada grupo
figura11 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(aes(color = species_group)) + 
  geom_smooth(method = "lm", aes(color = species_group))

# algunos detalles
figura12 <- ggplot(sw2, aes(x = height, y = mass)) +
  geom_point(aes(color = species_group)) + 
  geom_smooth(method = "lm", 
              aes(color = species_group),se = FALSE)


head(sw2)

sw2.ejemplo <- sw2[,c("name","height","mass","species_group")]
sw2.ejemplo$valor <- TRUE

sw3.ejemplo <- pivot_wider(data = sw2.ejemplo,
                           id_cols = c(name,height,mass),
                           names_from = species_group, 
                           values_from = valor,
                           values_fill = list(valor = FALSE))
head(sw3.ejemplo)

sw3.plot <- ggplot(sw3.ejemplo, aes(x = height, y = mass)) + 
  geom_point(aes(color = Human)) # esto sólo nos diferencia humanos de otros


eq <- read.csv2(file = "../data/Earthquake_data.csv",dec = ".",stringsAsFactors = FALSE)

# necesitamos crear una columna "century"
# en principio vacía
eq$century <- NA

# hay varias maneras de traducir el año a siglo. 
# Por lo que hemos visto hasta ahora, podemos hacerlo con un bucle, 
# que vaya fila por fila,
# calcule a qué siglo corresponde el año, 
# y rellene el valor de cada terremoto.

for(i in 1:nrow(eq)){
  
  my.century <- NA
  
  if(eq$Year[i] < 1500){
    my.century <- "< XVI"
  }else if(eq$Year[i] >= 1500 & eq$Year[i] < 1600){
    my.century <- "XVI"
  }else if(eq$Year[i] >= 1600 & eq$Year[i] < 1700){
    my.century <- "XVII"
  }else if(eq$Year[i] >= 1700 & eq$Year[i] < 1800){
    my.century <- "XVIII"
  }else if(eq$Year[i] >= 1800 & eq$Year[i] < 1900){
    my.century <- "XIX"
  }else if(eq$Year[i] >= 1900){
    my.century <- "XX"
  }
  
  eq$century[i] <- my.century
  
}

table(eq$century)

# lo más claro es crear un segundo dataframe sólo con esta información
terremotos_siglo <- data.frame(terremotos = table(eq$century))
# al usar el resultado de "table" para crear un dataframe, 
# generamos dos columnas, 
# una con el nombre del siglo y otra con el número de terremotos
terremotos_siglo
# los nombres del dataframe no son del todo correctos
names(terremotos_siglo) <- c("siglo","terremotos")
# y el orden no es adecuado, no debería ser alfabético. 
# En este caso, necesitamos un factor
terremotos_siglo$siglo <- factor(terremotos_siglo$siglo, 
                                 levels = c("< XVI", 
                                            "XVI", 
                                            "XVII", 
                                            "XVIII", 
                                            "XIX", 
                                            "XX"))

# a veces, R tiene comportamientos extraños. 
# Si no entendéis porqué hace falta "group = 1",
# no os preocupéis... yo tampoco.
# En el "cookbook" (enlace abajo), dice: 
# For line graphs, the data points must be grouped so that it knows 
# which points to connect. 
# In this case, it is simple – all points should be connected, so group=1.
figura13 <- ggplot(terremotos_siglo, aes(x = siglo, 
                                        y = terremotos, 
                                        group = 1)) +
  geom_line()

figura14 <- ggplot(terremotos_siglo, aes(x = siglo, 
                                        y = terremotos, 
                                        group = 1)) +
  geom_line(linetype = "dashed", size = 3, color = "darkred")

figura15 <- ggplot(sw2, aes(x = height, y = mass))+
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(species_group ~ .)

figura16 <- ggplot(sw2, aes(x = height, y = mass))+
  geom_point() + 
  facet_grid(gender~species_group)

figura17 <- ggplot(sw2, aes(x = height, y = mass))+
  geom_point(aes(color = species_group, shape = gender)) + 
  facet_grid(gender~species_group)


# primero, eliminamos los NA para mejorar la visualización
sw3 <- subset(sw2, !is.na(height) & !is.na(mass) & !is.na(gender))

figura18 <- ggplot(sw3, aes(x = height, y = mass))+
  geom_point(aes(color = species_group, shape = gender), size = 2) + 
  facet_grid(gender ~ species_group) + 
  # una nueva capa que especifica título y, opcionalmente, subtítulo
  ggtitle(label = "Peso y altura de personajes de Star Wars",
          subtitle = "según especie y género") + 
  # otra capa en la que especificamos el texto de los ejes
  labs(x = "altura (cm)", y = "peso (kg)") +
  # leyendas
  # color
  scale_color_manual(name = "Tipo de especie", 
                     labels = c("androide","humano", "otro", "Yoda"), 
                     values = c("grey30","darkorange",
                                "darkred","darkgreen")) +
  # shape (forma)
  scale_shape_manual(name = "Género", 
                     labels = c("Femenino", "Masculino", "Otro/ninguno"), 
                     values = c(1,5,6))

figura19 <- ggplot(sw3, aes(x = height, y = mass))+
  geom_point(aes(color = species_group, shape = gender), size = 2) + 
  facet_grid(gender ~ species_group) + 
  # una nueva capa que especifica título y, opcionalmente, subtítulo
  ggtitle(label = "Peso y altura de personajes de Star Wars",
          subtitle = "según especie y género") + 
  # otra capa en la que especificamos el texto de los ejes
  labs(x = "altura (cm)", y = "peso (kg)") +
  # leyendas
  # color
  scale_color_manual(name = "Tipo de especie", 
                     labels = c("androide","humano", "otro", "Yoda"), 
                     values = c("grey30","darkorange","darkred","darkgreen")) +
  # shape (forma)
  scale_shape_manual(name = "Género", 
                     labels = c("Femenino", "Masculino", "Otro/ninguno"), 
                     values = c(1,5,6)) + 
  # cambiamos al tema "bw", black and white
  theme_bw()


install.packages("patchwork")
library(patchwork)

figura.combinada <- figura9 + figura10


# bajamos el contorno de todos los países especificando "world" en map_data
world_map <- map_data("world")
# ¿qué es este objeto? simplemente un dataframe 
# con diferentes puntos de latitud y longitud
head(world_map)

# latitud y longitud en los ejes
mapamundi <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="gray") +
  theme_bw()

# vamos a añadir una capa de puntos que sea el punto 
# donde se registraron terremotos
# para esto necesitamos dibujar dos dataframes: 
# uno, world_map, nuestro mapa en formato polígono
# dos, los datos de los puntos asociados a terremotos.
# Esto se puede hacer en ggplot2 especificando, 
# en cada llamada a "geom_", qué datos usamos.

# en este caso dejamos la función principal sin argumentos
puntos_terremotos <- ggplot() + 
  geom_polygon(data = world_map,aes(x = long, 
                                    y = lat, 
                                    group = group), 
               fill = "gray") +
  geom_point(data = eq, aes(x = Lon, # cuidado con el nombre de columnas...
                            y = Lat), 
             color = "darkred", size = 1.2) + 
  theme_bw()

puntos_terremotos2 <- ggplot() + 
  # mapamundi
  geom_polygon(data = world_map,aes(x = long, 
                                    y = lat, 
                                    group = group), 
               fill = "gray") +
  # puntos
  geom_point(data = eq, aes(x = Lon, 
                            y = Lat, 
                            color = M), 
             size = 1.2) + 
  # tema
  theme_bw()

puntos_terremotos2 <- ggplot() + 
  # mapa
  geom_polygon(data = world_map,aes(x = long, y = lat, group = group), 
               fill = "gray") +
  # puntos
  geom_point(data = eq, aes(x = Lon, y = Lat, fill = M), 
             shape = 21, size = 2) + 
  # escala
  scale_fill_continuous(name = "magnitud", type = "viridis") + 
  # título
  ggtitle("Magnitud y localización de terremotos históricos")+
  # tema
  theme_bw()

puntos_terremotos2
