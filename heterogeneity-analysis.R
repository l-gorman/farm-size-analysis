library(brms)
library(rstan)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(sf)
library(sp)
library(stars)

# Setting the mac Graphics ttype 
X11(type = "cairo")
# options(device = "X11")


# Reading in Datasets
rhomis_geo_data <- readr::read_csv("./data/prepared-data/rhomis-geo-data.csv")

rhomis_geo_data <- sf::st_as_sf(x = rhomis_geo_data, wkt = "geometry")
rhomis_geo_data <- rhomis_geo_data[rhomis_geo_data$iso_3!="NER",]

ipums_all <- readr::read_csv("./data/prepared-data/ipums-all.csv")
ipums_all <- sf::st_as_sf(x = ipums_all, wkt = "geometry")
ipums_all <- ipums_all[ipums_all$iso_3!="NER",]

# lsms_all <- readr::read_csv("./data/prepared-data/lsms-all.csv")
# lsms_all <- sf::st_as_sf(x = lsms_all, wkt = "geometry")

world_all <- readr::read_csv("./data/prepared-data/world-shapefile.csv")
world_all <- sf::st_as_sf(x = world_all, wkt = "geometry")

bounding_box <- st_bbox(ipums_all)

# Plots
subnational_boundary_plot <- ggplot() +
  geom_sf(data=world_all, size=0.5) +
  geom_sf(data=ipums_all, color="black", size=0.1,aes(fill=iso_3))  +
  geom_sf(data=rhomis_geo_data, size=0.1) +
  coord_sf(xlim = c(bounding_box$xmin, bounding_box$xmax), ylim = c(bounding_box$ymin, bounding_box$ymax))
          

dir.create("./outputs/r_outputs/exploratory/",showWarnings =F ,recursive = T)
ggsave("./outputs/r_outputs/exploratory/subnational_boundaries.png", subnational_boundary_plot)


