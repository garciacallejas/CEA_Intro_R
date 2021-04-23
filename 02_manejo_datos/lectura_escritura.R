# 02 - Lectura/escritura

?read.table
?write.table


mi.archivo <- read.table(file = "data/starwars_names.csv", 
                         header = TRUE,
                         sep = ";")

head(mi.archivo)
str(mi.archivo)

a2 <- read.csv(file = "data/starwars_names_coma.csv")

a3 <- read.csv2(file = "data/starwars_names.csv")

# la orden "library" se usa para cargar un paquete determinado
library(readxl)
hoja1 <- read_excel(path = "data/hojas_excel.xlsx",sheet = "parcela_1")

library(readr)
?read_csv2

getwd()

?setwd

?write.table

a2$name[1] <- "Lucas"

# guardar archivo csv separado por punto y coma
# write.table es la función general
# podemos especificar si queremos una columna que guarde 
# los nombres de las filas de nuestro dataframe

# estas dos órdenes son equivalentes
# write.table(x = a2,file = "data/archivo_nuevo.csv",
#             sep = ";",
#             row.names = FALSE)
# write.csv2(x = a2,file = "data/archivo_nuevo.csv",
#            row.names = FALSE)

# y para guardar un archivo csv separado por comas, usamos
# write.csv(x = a2, file = "data/archivo_nuevo_comas.csv", 
#           row.names = FALSE)


# install.packages("xlsx")
# library(xlsx)
# ?write_xlsx

my.file <- "data/sample500tuits_starwarsandaluz.txt"
conn <- file(my.file,open = "r")
lineas <-readLines(conn)
