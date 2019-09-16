# 1. Representación simple de datos espaciales

# 1.1. Datos tipo vector

# Cargar librerias 
library(pacman)
pacman::p_load(raster, sf, maptools, rgdal, ggplot2, tidyverse, broom)

# Creación de 10 estaciones climaticas (llamadas de A a J)
nombre <- LETTERS[1:10]
longitud <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
              -120.8, -119.5, -113.7, -113.7, -110.7)
latitud <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
             36.2, 39, 41.6, 36.9)
est_climatic<- cbind(longitud, latitud)

# Simulación de datos de precipitación
set.seed(0)
precip <-round((runif(length(latitud))*10)^3)

nivel_precip <- 1 + precip/500
plot(est_climatic, cex=nivel_precip, pch=20, col='blue', main='Precitación por estaciones')
# Adicionar etiquetas
text.default(est_climatic, nombre, pos = 4)
# Adicionar leyenda
breaks <- c(100, 250, 500, 1000)
legend.psize <- 1+breaks/500 
legend("topright", legend=breaks, pch=20, pt.cex=legend.psize, col='blue', bg='gray')

# Adicionar puntos, lineas y polígonos al plot.
longitud <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitud <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
               36.2, 39, 41.6, 36.9)
x <- cbind(longitud, latitud)
plot(est_climatic, main='Precipitación por estaciones')
polygon(x, col='blue', border='light blue')
lines(est_climatic, lwd=3, col='red')
points(x, cex=2, pch=20)
points(est_climatic, cex=nivel_precip, pch=20, col='red', main='Precipitation by station') 

# Tabla de datos
tabla <- data.frame(longitud, latitud, nombre, precip)
tabla

# 1.2. Datos tipo raster

# Crear esqueleto de una base de datos raster
rast <- raster(ncol=10, nrow=10, xmx=-80, xmn=-150, ymn=20, ymx=60)
rast

# Asignar valores a objetos tipo raster
values(rast) <- runif(ncell(rast))
rast

# Asignar el número de celdas 
values(rast) <- 1:ncell(rast)
rast
plot(rast)







  
  