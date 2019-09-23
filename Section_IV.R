#########################################################################
# 4. Haciendo mapas con R: desde mapas estáticos hacia aplicaciones web #
#########################################################################

# 4.1 Haciendo mapas empleando datos tipo raster #

# Definir el directorio de trabajo
# en Windows sería algo a esto setwd("C:/Users/gusahu/Google Drive/Workshop_socher/unit_2")
setwd("/Users/gusahu/Google Drive/Workshop_GIS_with_R/Section_IV/Raster_data")

# Cargar librerias
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, tidyverse, RColorBrewer, ggpubr)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargar datos de precipitación y shapefile de municipios
# Precipitación acumulada anual
grids<-list.files("./hg85pr50_5", pattern = ".tif", full.names = TRUE)
pr_stack<- stack(grids)
pr_total <- sum(pr_stack)
mps <- shapefile('./mpiosColombia/mpios_geo_ok.shp')

# cortar raster al área del polígono
pr_col<- crop(pr_total,mps)
plot(pr_col)

# Procesando los datos
dpt <- aggregate(mps, 'NOMBRE_DPT')
gjr <- dpt[dpt@data$NOMBRE_DPT %in% "LA GUAJIRA",]
ch <- dpt[dpt@data$NOMBRE_DPT %in% "CHOCÓ",]

pr_gjr <- raster::crop(pr_col, gjr) %>% 
  raster::mask(., gjr) %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  mutate(dpto = 'La Guajira')
pr_ch <- raster::crop(pr_col, ch) %>% 
  raster::mask(., ch) %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  mutate(dpto = 'Choco')
pr <- rbind(pr_gjr, pr_ch) 

pr %>% filter(dpto == 'La Guajira') %>% pull(layer) %>% max()
pr %>% filter(dpto == 'Choco') %>% pull(layer) %>% max()

map_grj <- ggplot(pr %>% filter(dpto == 'La Guajira'))  +
  geom_tile(aes(x = x, y =  y, fill = layer)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "BuGn"), 
                       na.value = 'white', limits = c(0, 9700), breaks = seq(0, 9700, 2000)) +
  geom_polygon(data = dpt, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  ggtitle('La Guajira') +
  theme_bw() +
  coord_equal(xlim = extent(gjr)[1:2], ylim = extent(gjr)[3:4]) +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Pr') +
  theme(legend.position = 'none')
map_ch <- ggplot(pr %>% filter(dpto == 'Choco'))  +
  geom_tile(aes(x = x, y =  y, fill = layer)) +
  ggtitle('Chocó') +
  geom_polygon(data = dpt, aes(x=long, y = lat, group = group), color = 'grey', fill='NA') +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "BuGn"), 
                       na.value = 'white', limits = c(0, 9700), breaks = seq(0, 9700, 2000)) +
  theme_bw() +
  coord_equal(xlim = extent(ch)[1:2], ylim = extent(ch)[3:4]) +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Prec') +
  theme(legend.position = 'right')

maps2 <- gridExtra::grid.arrange(map_grj, map_ch, ncol = 2)

ggsave(plot = maps2, filename = './mapa_prec2.png', width = 12, height = 9, units = 'in', dpi = 300)


# 4.2 Haciendo mapas empleando datos tipo vector#

# Definir el directorio de trabajo
# en Windows sería algo a esto setwd("C:/Users/gusahu/Google Drive/Workshop_socher/unit_2")
setwd("/Users/gusahu/Google Drive/Workshop_socher/Make_maps/Vector_data")

# eliminar objetos del espacio de trabajo
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cargar librerias
library(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, stringr, sf, broom)

shp <- shapefile('./meanTemp/mean_temp.shp')
shp@data$MEAN
shp@data$NOM_MUNICI
shp@data$NOMBRE_DPT
dpto <- unique(shp@data$NOMBRE_DPT)
dpto

# convertir shapefile en una tabla
tdy <- broom::tidy(shp, 'ID_ESPACIA')
dfm <- tibble(id_espacial = shp@data$ID_ESPACIA, mpio = shp@data$NOM_MUNICI, temp = shp@data$MEAN)
dfm <- inner_join(tdy, dfm, by = c('id' = 'id_espacial'))

# obtener los límites geográficos de los paises vecinos
pnm <- raster:: getData('GADM', country = 'PAN', level = 0)
ecu <- raster:: getData('GADM', country = 'ECU', level = 0)
ven <- raster:: getData('GADM', country = 'VEN', level = 0)
per <- raster:: getData('GADM', country = 'PER', level = 0)
bra <- raster:: getData('GADM', country = 'BRA', level = 0)

# importaar archivos .rds
pnm <- readRDS(file = "gadm36_PAN_0_sp.rds")
ecu <- readRDS(file = "gadm36_ECU_0_sp.rds")
ven <- readRDS(file = "gadm36_VEN_0_sp.rds")
per <- readRDS(file = "gadm36_PER_0_sp.rds")
bra <- readRDS(file = "gadm36_BRA_0_sp.rds")

pnm <- broom::tidy(pnm, 'NAME_0')
ecu <- broom::tidy(ecu, 'NAME_0')
ven <- broom::tidy(ven, 'NAME_0')
per <- broom::tidy(per, 'NAME_0')
bra <- broom::tidy(bra, 'NAME_0')

# Mapa temperatura promedio en colombia año 2015 
map <- ggplot()+
  geom_polygon(data = dfm, aes(x = long, y = lat, group = id, fill = temp), colour = 'grey') +
  scale_fill_distiller(palette = 'RdYlGn') +
  geom_polygon(data = pnm, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = ecu, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = ven, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = per, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = bra, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  coord_equal(xlim = c(-80, -66), ylim = c(-4.22, 13)) +
  xlab('Longitud') +
  ylab('Latitud') +
  labs(fill = 'Temperatura (c)') +
  theme(legend.justification = c(0,0),
        legend.position = c(0.005, 0.005),
        legend.background = element_rect(fill = alpha('white', 1), colour = alpha('white', 0.4)))
ggsave(plot = mapa, filename = 'mapa_temp.png', units = 'in', width = 6, height = 7.5, dpi = 300)
