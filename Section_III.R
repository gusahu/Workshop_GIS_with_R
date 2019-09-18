# 3. Datos de ráster geográfico en R

# Datos obtenidos del WorldClim - Global Climate Data http://www.worldclim.org/

# 3.1. Manipulación de archivos tipo raster

# Definir el directorio de trabajo
# En Windows sería algo a esto setwd("C:/Users/gusahu/Google Drive/Workshop_GIS_with_R/Section_III")
setwd("/Users/gusahu/Google Drive/Workshop_GIS_with_R/Section_III")

# Cargar librerias
library(pacman)
pacman::p_load(raster, rgdal, maptools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Importar archivos tipo raster
pr_mayo<- raster('./hg85pr50/hg85pr505.tif')
plot(pr_mayo)
tx_mayo<- raster('./hg85tx50/hg85tx505.tif')
plot(tx_mayo)

#Cargar capa de polígono
col<- readOGR('./shp_dpto/deptos_wgs84.shp')

#Cortar raster al área del polígono
tx_mayo_col<- crop(tx_mayo,col)

#Plotear raster 
plot(tx_mayo_col, main = "Tmax. 2050 HadGEM CC RCP 8.5-Colombia (c*10)") 
# Adicionar polígono al raster
plot(col, add=TRUE)

# 3.2 Operaciones entre rásteres - cálculo Indice de Aridez de Martone (1926)

# Primer paso: cargar capas ráster
# cargar raster por lotes
grids<-list.files("./hg85tn50", pattern = ".tif", full.names = TRUE)  # temperatura mínima 
grids2<-list.files("./hg85tx50", pattern = ".tif", full.names = TRUE) # temperatura máxima
grids3<-list.files("./hg85pr50", pattern = ".tif", full.names = TRUE) # precipitación

# crear raster stack 
tn_stack<- stack(grids)
tx_stack<- stack(grids2)
pr_stack<- stack(grids3)

# Segundo paso: operar rásteres

tn_promedio <- ((sum(tn_stack))/120) # temperatura minima promedio anual
tx_promedio <- ((sum(tx_stack))/120) # temperatura maxima promedio anual
tm_promedio <- mean(tn_promedio, tx_promedio) # cáulo temperatura media anual
pr_total <- sum(pr_stack) # precipitación acumulada anual


# Tercer paso: cargar una capa vectorial de polígono

col<- readOGR('./shp_dpto/deptos_wgs84.shp')

# Cuarto paso: cortar raster al área del polígono

pr_total_col<- crop(pr_total,col)
plot(pr_total_col)  # plot precipitación anual acumulada

tm_promedio_col<- crop(tm_promedio,col)
plot(tm_promedio_col) # plot temperatura media anual

# Quinto paso: calcular el Índice de Aridez de Martonne para el año 2050

ind_a <- pr_total_col / (tm_promedio_col + 10) 
plot(ind_a, main = "Índice de Aridez de Martonne año 2050-Colombia")
plot(col, add=TRUE) # adicionar capa vectorial de polígono

# exportar a *.tif
writeRaster(ind_a, filename = "Ia_2050", format="GTiff", overwrite=TRUE)

