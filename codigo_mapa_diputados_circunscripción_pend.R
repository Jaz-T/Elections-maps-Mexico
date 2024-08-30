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

colores_circunscripción <- c(circuns_1 = "#5c8e79", circuns_2 = "#7ca493", circuns_3 = "#9cbaad", circuns_4 = "#bcd1c7", circuns_5 = "#dde8e3")

#Directorio de trabajo para cargar mapa de méxico

#Datos mapas GADM version 2.8
#https://gadm.org/download_country_v2.html
setwd("I:/E01 Dir Apoyo a Operaciones/Investigación DAOB/TOPICS/ELECCIONES/Mexico 2024/Elecciones gobernadores")
#Cargar mapa
#Leer base de datos con coordenadas por Estado 
forma_estados <- readShapeSpatial("./MEX_adm_shp/MEX_adm1.shp") 
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

#Coordenadas circuns

# circuns_dat <- estados_dat %>% 
#   mutate(long = if_else(group2_circuns == 0, NA, long),
#          lat = if_else(group2_circuns == 0, NA, lat))
#geom_polygon(data = circuns_dat, aes(long, lat, group=group),colour="darkgrey", fill= "#E1EDE0") +

#Graficar mapa de la república

mex <- ggplot(data = estados_dat, aes(long, lat, group=group)) + 
  geom_polygon(colour=colores_circunscripción[estados_dat$circuns], fill= colores_circunscripción[estados_dat$circuns]) +
  coord_map(projection="mercator") +
  theme_nothing() 


#Cargar archivo
diputados.dots.circuns <- read.csv("diputados_dots_circuns.csv", sep = ",", header = T, stringsAsFactors = F, fileEncoding="latin1")

#Convertir coordenadas en elementos numéricos
diputados.dots.circuns$long <- as.numeric(diputados.dots.circuns$long)
diputados.dots.circuns$lat <- as.numeric(diputados.dots.circuns$lat)

#Convertir partido ganador en un factor
diputados.dots.circuns$diputado_partido <- factor(diputados.dots.circuns$diputado_partido, 
                                 levels = c("PAN", "PRI", "PRD", "MORENA", "PT", "PVEM", "MC", "OTROS"))

diputados_mapa_circuns <- mex +
  geom_point(data = diputados.dots.circuns,
             aes(x=long, y=lat, color=diputado_partido), 
             size=2, alpha=I(0.7),
             inherit.aes = FALSE) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_manual("",
                     values = colores)  

diputados_mapa_circuns

