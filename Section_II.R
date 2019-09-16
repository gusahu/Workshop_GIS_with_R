# 2. Datos de vectores geográficos en R


# Definir directorio de trabajo
# en Windows sería algo a esto setwd("C:/Users/gusahu/Google Drive/Workshop_socher/Section_II")
setwd("/Users/gusahu/Google Drive/Workshop_GIS_with_R/Section_II")

# Cargar librerias
library(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, stringr, sf)

# Limpiar ambiente de trabajo
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Importar archivos shape (archivos tipo vector)
mps <- shapefile('./data/mpios_geo_ok2.shp')

# Plottear la geometría o el archivo .shp
plot(mps)

# Realizar la unión (join) de una tabla con un shapefile
# importar archvios shape (archivos tipo vector)
shp <- st_read('./data/mpios_geo_ok.shp')
shp

# Importar archivos en formato .csv (.xls)
tbl <- read_csv('./data/produccion_cacao.csv')
tbl

# Años disponible en archivo .csv
yrs <- unique(tbl$PERIODO)
yrs
tbl <- tbl %>% # pipe (%>%) es un operador que permite encadenar funciones
  filter(PERIODO == 2017) %>% 
  mutate(MPIO = iconv(MPIO, to = 'latin1')) # transformación de variables en un data frame 

# Ver atributos de objetos
str(tbl)
str(shp)

# Transformación ID espacial
shp <- shp %>% 
  mutate(ID_ESPACIA = as.numeric(as.character(ID_ESPACIA)))

# Realizar unión entre una table y un shapefile
fnl <- inner_join(x = shp, y = tbl, by = c('ID_ESPACIA' = 'COD_MUNI'))
fnl %>% 
  dplyr::select(PRODUCCION) %>% 
  plot() 

# Exportar el nuevo archivo espacial con las nuevas características
st_write(obj = fnl, dsn = './_shp', layer = 'producción_cacao', driver = 'ESRI shapefile', update = TRUE )
plot(fnl, add=T)
