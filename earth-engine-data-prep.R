library(readr)
library(dplyr)
library(tibble)
library(jsonlite)

# Spatial Packages
library(sf)
library(sp)
library(stars)
library(geojsonsf)

library(ggplot2)


fao_level_2 <- geojson_sf('data/earth-engine/level-2-variables/fao-gaul-level-2.geojson')
fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
fao_level_2 <-st_set_crs(fao_level_2,'WGS 84')


world_all <- readr::read_csv("./data/prepared-data/world-shapefile.csv")
world_all <- sf::st_as_sf(x = world_all, wkt = "geometry")

ggplot() +
  geom_sf(data=world_all, size=0.5) +
  geom_sf(data=fao_level_2, color="black", size=0.1,aes(fill=ADM0_NAME)) 
