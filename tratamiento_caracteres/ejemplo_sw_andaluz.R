
# ejemplo de tratamiento de datos para un archivo .txt con el contenido
# de una serie de tuits

# para que entendáis que stackoverflow es el mejor amigo de todo 
# usuario de R...
# ¿qué preguntas he usado para ayudarme con este script?
# https://stackoverflow.com/questions/16816032/convert-named-character-vector-to-data-frame
# https://stackoverflow.com/questions/7597559/grep-using-a-character-vector-with-multiple-patterns
# https://stackoverflow.com/questions/55171086/r-regex-extract-words-beginning-with-symbol
# https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-by-line
# https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

# leemos un archivo linea a linea
my.file <- "/home/david/Work/Projects/R_courses/CEA2020_Intro/data/sample500tuits_starwarsandaluz.txt"
conn <- file(my.file,open = "r")
lineas <-readLines(conn)

df.lineas <- as.data.frame(lineas)

library(tidyverse) # includes the package stringr

is.RT <- grep("RT|Retweeted",df.lineas$lineas)

df.lineas$is.RT <- FALSE
df.lineas$is.RT[is.RT] <- TRUE

# ¿cuántos usuarios aparecen en cada tuit?
# primero, extraemos una lista con todos los usuarios en cada tuit
users.mentioned <- str_extract_all(df.lineas$lineas, "(?<=^|\\s)@[^\\s]+")
# cuántos elementos tiene cada posición de la lista
users.mentioned2 <- lapply(users.mentioned,FUN = length)
# convertimos esto a un vector
num.users <- unlist(users.mentioned2)
# y este número lo pasamos a una columna del dataframe
df.lineas$users.mentioned <- num.users

# ¿qué hashtags aparecen y con qué frecuencia?
# extraemos todas las palabras que comiencen por @
hashtags <- str_extract_all(df.lineas$lineas, "(?<=^|\\s)#[^\\s]+")
# limpiamos los signos de puntuación (esto elimina también la # al comienzo)
clean.hashtags <- lapply(hashtags, FUN = function(x)str_replace_all(x, "[[:punct:]]", ""))
# convertimos la lista en un vector
clean.hashtags.vector <- unlist(clean.hashtags)
# extraemos las frecuencias usando la función "table" y convirtiéndolo en
# un dataframe
hashtag.freq <- as.data.frame(table(clean.hashtags.vector))
# renombramos las columnas del dataframe
names(hashtag.freq) <- c("hashtag","frecuencia")

# creamos un gráfico de barras
freq.plot <- ggplot(hashtag.freq, aes(x = hashtag, y = frecuencia)) + 
  geom_col() + 
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8, hjust = 1))
freq.plot
