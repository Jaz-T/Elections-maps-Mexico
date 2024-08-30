#Librerías

library(tidyverse)
library(readr)
library(knitr)
library(ggplot2)
library(ggmap)
library(maptools)
library(car)
library(mapproj)
library(scales)

####Establecer códigos de gama de colores para los partidos 

colores <- c(PAN = "#00529B", PRI = "#FF0000", PRD = "#E3E647", 
             MORENA = "#C4351C", PT = "#B9935A", PVEM = "#33CC00", MC = "#FF8201", OTROS = "#6A6E73")


#Directorio de trabajo para cargar mapa de méxico

#Datos mapas GADM version 2.8
#https://gadm.org/download_country_v2.html
setwd("I:/E01 Dir Apoyo a Operaciones/Investigación DAOB/TOPICS/ELECCIONES/Mexico 2024/Elecciones gobernadores")
#Cargar mapa
#Leer base de datos con coordenadas por Estado 
forma_estados <- readShapeSpatial("./MEX_adm_shp/MEX_adm1.shp") 

#Extraer coordenadas para etiquetas
#coord.poly <- forma_estados@polygons 
#coord <- data.frame(ID = seq(1:32), 
                  #  long = seq(1:32),
                  #  lat = seq(1:32))
#Función para extraer coordenadas centrales
#for (i in 1:32) {
 # coord[i, 2:3] <- t(coord.poly[[i]]@labpt)
#}
#write.csv(coord, "coordenadas_mx.csv")
#Convertir base en data frame
estados_dat <- fortify(forma_estados)

#Directorio de trabajo bases diputados
setwd("I:/E01 Dir Apoyo a Operaciones/Investigación DAOB/TOPICS/ELECCIONES/Mexico 2024/resultados_elecciones_2024_reporte/diputados")

#Cargar diccionario para los ids por estado
diccionario_ids <- read.csv("coordenadas_mx_etiquetas.csv", sep = ",", header = T, stringsAsFactors = F, fileEncoding="latin1")
diccionario_ids$id <- diccionario_ids$ID_mapa
#Transformar ids para que sean compatibles con las del INEGI
estados_dat$id <- as.numeric(estados_dat$id)+1 
estados_dat <- estados_dat %>% 
  right_join(diccionario_ids, by = "id", suffix = c("",".labs")) #ID es la etiqueta que corresponde a la de INEGI

#Graficar mapa de la república

mex <- ggplot(data = estados_dat, aes(long, lat, group=group)) + 
  geom_polygon(colour='darkgrey', fill= "#E1EDE0") +
  coord_map(projection="mercator") +
  theme_nothing() 


#Cargar archivo
diputados.dots <- read.csv("diputados_dots.csv", sep = ",", header = T, stringsAsFactors = F, fileEncoding="latin1")

#Convertir coordenadas en elementos numéricos
diputados.dots$long <- as.numeric(diputados.dots$long)
diputados.dots$lat <- as.numeric(diputados.dots$lat)

#Convertir partido ganador en un factor
diputados.dots$diputado_partido <- factor(diputados.dots$diputado_partido, 
                                 levels = c("PAN", "PRI", "PRD", "MORENA", "PT", "PVEM", "MC", "OTROS"))

diputados_mapa <- mex +
  geom_point(data = diputados.dots,
             aes(x=long, y=lat, color=diputado_partido), 
             size=2, alpha=I(0.7),
             inherit.aes = FALSE) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_manual("",
                     values = colores)  

diputados_mapa

