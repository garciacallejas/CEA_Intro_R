
library(tidyverse)

tt <- starwars

films <- tt$films
veh <- tt$vehicles
ss <- tt$starships

unique.films <- unique(unlist(films))
unique.names <- unique(tt$name)
unique.vehicles <- unique(unlist(veh))
unique.starships <- unique(unlist(ss))

film.char <- data.frame(name = unique.names, f1 = FALSE, 
                        f2 = FALSE, f3 = FALSE, f4 = FALSE, f5 = FALSE, f6 = FALSE,
                        f7 = FALSE, stringsAsFactors = FALSE)
names(film.char)[2:8] <- unique.films

for(i.name in 1:nrow(film.char)){
  my.films <- films[[i.name]]
  film.char[i.name,my.films] <- TRUE
}

film.veh <- data.frame(name = unique.names, f1 = FALSE, 
                        f2 = FALSE, f3 = FALSE, f4 = FALSE, f5 = FALSE, f6 = FALSE,
                        f7 = FALSE, f8 = FALSE,f9 = FALSE,f10 = FALSE,stringsAsFactors = FALSE)
names(film.veh)[2:11] <- unique.vehicles

for(i.name in 1:nrow(film.veh)){
  my.ve <- veh[[i.name]]
  film.veh[i.name,my.ve] <- TRUE
}

film.ss <- data.frame(name = unique.names, f1 = FALSE, 
                       f2 = FALSE, f3 = FALSE, f4 = FALSE, f5 = FALSE, f6 = FALSE,
                       f7 = FALSE, f8 = FALSE,f9 = FALSE,f10 = FALSE,
                      f11 = FALSE, f12 = FALSE,f13 = FALSE,f14 = FALSE,
                      f15 = FALSE, f16 = FALSE, stringsAsFactors = FALSE)
names(film.ss)[2:17] <- unique.starships

for(i.name in 1:nrow(film.ss)){
  my.ss <- ss[[i.name]]
  film.ss[i.name,my.ss] <- TRUE
}

write.csv2(tt,"data/starwars_info_personajes.csv",row.names = FALSE)
write.csv2(film.char,"data/starwars_personajes_peliculas.csv",row.names = FALSE)
write.csv2(film.veh,"data/starwars_personajes_vehiculos.csv",row.names = FALSE)
write.csv2(film.ss,"data/starwars_personajes_naves.csv",row.names = FALSE)


# earthquakes -------------------------------------------------------------

eq <- read.csv2(file = "data/Earthquake_data.csv",dec = ".",header = TRUE,stringsAsFactors = FALSE)

eq.clean <- eq[,c("En","Year","Mo","Da","Ho","Mi","Se","Area","Lat","Lon","Dep","M")]

eq.wide.1 <- eq[,c("Year","Area","M")]
eq.wide.1 <- unique(eq.wide.1)
eq.wide <- spread(eq.wide.1[1:50,],key = Year, value = M)

write.csv2(eq.wide,file = "./data/Earthquake_wide_example.csv",row.names = FALSE)
