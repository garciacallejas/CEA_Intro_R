knitr::opts_chunk$set(echo = TRUE, results = FALSE, warning = FALSE, message = FALSE)

# cargamos el paquete
library(tidyverse)

# trabajaremos con los datos de terremotos
eq <- read.csv2(file = "../data/Earthquake_data.csv",
                dec = ".",
                stringsAsFactors = FALSE)

eq.clean <- eq[,c("En","Year","Mo","Da","Ho",
                  "Mi","Se","Area","Lat","Lon",
                  "Dep","M")]

# el encadenamiento se codifica con la orden %>%
# estas dos sintaxis son equivalentes
summary(eq.clean)
eq.clean %>% summary()

eq.clean %>%
  drop_na() %>%
  nrow()

eq.clean %>%
  filter(!is.na(Dep)) %>%
  summary()


valores.medios <- iris %>% 
  group_by(Species) %>% 
  summarise(mean.sepal.length = mean(Sepal.Length),
            mean.petal.length = mean(Petal.Length))

iris2 <- iris %>%
  mutate(columna.nueva = Sepal.Length * Sepal.Width)

# necesitamos estos dos paquetes
library(sp)
library(rworldmap)

# points es un dataframe con valores de longitud (columna 1) y latitud (c2)
coords2country <- function(points){  
  countriesSP <- rworldmap::getMap(resolution='low')

  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, 
                               proj4string=sp::CRS(sp::proj4string(countriesSP)))  

  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)

  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

coords2continent <- function(points){  
  countriesSP <- rworldmap::getMap(resolution='low')

  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, 
                               proj4string=sp::CRS(sp::proj4string(countriesSP)))  

  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)

  # return the ADMIN names of each country
  # indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

eq.paises <- eq.clean %>% 
  mutate(country = coords2country(data.frame(Lon,Lat)),
         continent = coords2continent(data.frame(Lon,Lat)))

table(eq.paises$continent)
table(eq.paises$country)

magnitud.eurasia <- eq.paises %>% 
  filter(continent == "Eurasia") %>%
  summarise(mean.magnitude = mean(M),
            sd.magnitude = sd(M),
            min.year = min(Year))

# podemos repetir el mismo análisis para todos los continentes
magnitud.continentes <- eq.paises %>%
  group_by(continent )%>%
  summarise(mean.magnitude = mean(M),
            sd.magnitude = sd(M),
            num.eq = n(),
            min.year = min(Year))


library(tidytext)

tuits <- read.delim("../data/tuits_madrid_16a.csv",stringsAsFactors = FALSE)

# extraemos cada palabra de los tuits de cada usuario
tweet_words <- tuits %>% 
  select(id.tweet, text) %>% 
  unnest_tokens(word,text)

# seleccionamos palabras de más de 3 caracteres y eliminamos algunas irrelevantes
invalid_words <- c("none","https","t.co","twitter.com","status","false",
                   "images","twitter","pbs.twimg.com","profile_images",
                   "están","esto","para","android")
tweet_words_2 <- tweet_words %>% 
  filter(nchar(word) > 3) %>%
  filter(!word %in% invalid_words)
  
# qué palabras son las 20 más repetidas
tweet_words_2 %>% count(word,sort=T) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
    n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab("")


# necesitamos un léxico en castellano
lexico <- read.csv("https://bitsandbricks.github.io/data/sdal.csv", 
                   stringsAsFactors = FALSE)

# igualamos el nombre de la columna "palabra" para unir los datasets
head(tweet_words_2)
names(tweet_words_2)[2] <- "palabra"

sa <- tweet_words_2 %>% left_join(lexico)
sa.long <- pivot_longer(sa,cols = c(media_agrado,media_activacion,media_imaginabilidad),names_to = "sentimiento",values_to = "media")

sa.conteo <- sa.long %>% filter(!is.na(media)) %>% group_by(sentimiento) %>% summarise(total=sum(media))

sa.plot <- ggplot(sa.conteo, aes(x = sentimiento, y = total)) +
  geom_col(aes(stat = "identity")) + 
  theme(axis.text.x  = element_text(angle = 60, hjust = 1))

