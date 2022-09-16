#' Exploring and Analysing Farm-Size Dsitributions
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tibble)
library(jsonlite)
library(tidyr)

# Spatial Packages
library(sf)
library(sp)
library(stars)
library(geojsonsf)
library(corrplot)
library(raster)
library(leaflet)
library(mapview)

# Graph Plotting
library(ggplot2)
library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(GGally)



# Reading Data ------------------------------------------------------------
# RHoMIS Data
# rhomis_data <- readr::read_csv("./data/prepared-data/rhomis-ee-gaez.csv")
# rhomis_data <- rhomis_data[!is.na(rhomis_data$gps_lat) & !is.na(rhomis_data$gps_lon),]
# rhomis_data <- st_as_sf(rhomis_data, coords = c("gps_lon", "gps_lat"), 
#                         crs = 4326, agr = "constant", remove = F)

indicator_data <- readr::read_csv("./data/prepared-data/rhomis-indicator-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$gps_lat) & !is.na(indicator_data$gps_lon),]
indicator_data_geo <- st_as_sf(indicator_data, coords = c("gps_lon", "gps_lat"), 
                        crs = 4326, agr = "constant", remove = F)



# FAO administrative data
fao_level_2 <- geojson_sf('data/prepared-data/fao_level_2.geojson')
fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
fao_level_2_geo <-st_set_crs(fao_level_2,'EPSG:4326')

land_categories <-  readr::read_csv("./data/prepared-data/land_cover_classes.csv")

# Data Cleaning -----------------------------------------------------------


# Removing Null values

#
indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[!is.na(indicator_data$AEZ_Classes_57),]

land_cat_columns <- paste0("land_cat_",c(1:17))
indicator_data[land_cat_columns] <- lapply(indicator_data[land_cat_columns] , function(column){
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/indicator_data$pixelCount
  return(column)
}) %>% dplyr::bind_cols()


new_land_cat_columns <- land_categories$Tag
colnames(indicator_data)[colnames(indicator_data) %in% land_cat_columns] <- new_land_cat_columns





# Outlier Detection -------------------------------------------------------


outlier_filter <- quantile(indicator_data[["land_cultivated_ha"]], probs = c(0.01,0.99))

table(indicator_data[c("land_cultivated_ha")]==outlier_filter[1]) # 236 with 0 land cult (investigate further)
table(indicator_data[c("land_cultivated_ha")]>outlier_filter[2]) # 201 with land cult greater than 99th percentile (24th)


indicator_data <- indicator_data[indicator_data[c("land_cultivated_ha")] != outlier_filter[1] & indicator_data[c("land_cultivated_ha")] <= outlier_filter[2],]


colnames(indicator_data)[colnames(indicator_data) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(indicator_data)[colnames(indicator_data) == "b1_mean_mean"] <- "nightlights"
colnames(indicator_data)[colnames(indicator_data) == "population_density_mean_mean"] <- "population_density"

colnames(indicator_data)[colnames(indicator_data) == "elevation_mean"] <- "elevation"
colnames(indicator_data)[colnames(indicator_data) == "NDVI_mean_mean"] <- "ndvi"
colnames(indicator_data)[colnames(indicator_data) == "constant_mean"] <- "topographic_diversity"





# Plotting Correlations --------------------------------------------------

x <- c("healthcare_traveltime",
       "nightlights",
        "population_density",
       
       "elevation",
       "ndvi",
       "topographic_diversity",
       "adjusted_length_growing_period",
       "AEZ_Classes_33",
       "AEZ_Classes_57",
       new_land_cat_columns
       )

y <-c("land_cultivated_ha")


cor(indicator_data[c(x,y)])

corr_matrix <- round(cor(indicator_data[c(x,y)]),2) %>% tibble::as_tibble()
corr_matrix$var <- colnames(corr_matrix)
corr_matrix <- corr_matrix %>% pivot_longer(cols = colnames(corr_matrix)[colnames(corr_matrix)!="var"])

colnames(corr_matrix) <- c("var1", "var2", "value")

ggplot(data = corr_matrix, aes(x=var1, y=var2, fill=value)) + 
  geom_tile() +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                    hjust = 1))

land_cult_corr <- corr_matrix[corr_matrix$var1=="land_cultivated_ha", ]
land_cult_corr <- land_cult_corr[land_cult_corr$var2!="land_cultivated_ha",]

land_cult_corr$var2 <- factor(land_cult_corr$var2, levels=x, ordered = T)
land_cult_corr$factor_level <- as.numeric(land_cult_corr$var2)

socio_economic <- c("healthcare_traveltime",
                    "nightlights",
                    "population_density")

environmental <- c("elevation",
"ndvi",
"topographic_diversity",
"adjusted_length_growing_period")

aes <-  c("AEZ_Classes_33",
"AEZ_Classes_57")

land_cat <- new_land_cat_columns


min_social <- min(land_cult_corr$factor_level[land_cult_corr$var2 %in% socio_economic]) -0.5
max_social <- max(land_cult_corr$factor_level[land_cult_corr$var2 %in% socio_economic]) + 0.5

min_env <- min(land_cult_corr$factor_level[land_cult_corr$var2 %in% environmental]) -0.5
max_env <- max(land_cult_corr$factor_level[land_cult_corr$var2 %in% environmental]) + 0.5

min_class <- min(land_cult_corr$factor_level[land_cult_corr$var2 %in% land_cat]) -0.5
max_class <- max(land_cult_corr$factor_level[land_cult_corr$var2 %in% land_cat]) + 0.5

ggplot(data = land_cult_corr, aes(y=var2, x=value)) + 
  geom_segment( aes(x=0, xend=value, y=var2, yend=var2), color="black")+
 geom_point( color="orange", size=2, aes(x=value)) +
  coord_cartesian(xlim = c(-0.5, 0.5), # This focuses the x-axis on the range of interest
                  ylim = c(-0.2, length(land_cult_corr$factor_level)),
                  clip = 'off')+
   geom_segment( mapping=aes(x=-0.93, xend=-0.93, y=min_social, yend=max_social,)) + 
  geom_segment( mapping=aes(x=-0.93, xend=-0.91, y=min_social, yend=min_social)) + 
  geom_segment( mapping=aes(x=-0.93, xend=-0.91, y=max_social, yend=max_social)) + 
  geom_segment( mapping=aes(x=-0.93, xend=-0.91, y=max_social, yend=max_social)) + 
  geom_segment( mapping=aes(x=-0.93, xend=-0.95, y=(max_social + min_social)/2, yend=(max_social + min_social)/2)) +
  geom_text(aes(x = -1.02, y = (max_social + min_social)/2, label = "Social")) +
  
  geom_segment( mapping=aes(x=-1.1, xend=-1.1, y=min_env, yend=max_env)) + 
  geom_segment( mapping=aes(x=-1.1, xend=-1.08, y=min_env, yend=min_env)) + 
  geom_segment( mapping=aes(x=-1.1, xend=-1.08, y=max_env, yend=max_env)) + 
  geom_segment( mapping=aes(x=-1.1, xend=-1.12, y=(max_env + min_env)/2, yend=(max_env + min_env)/2)) +
  geom_text(aes(x = -1.27, y = (min_env + max_env)/2, label = "Environmental")) +
  
  
  geom_segment( mapping=aes(x=-0.95, xend=-0.95, y=min_class, yend=max_class)) + 
  geom_segment( mapping=aes(x=-0.95, xend=-0.93, y=min_class, yend=min_class)) + 
  geom_segment( mapping=aes(x=-0.95, xend=-0.93, y=max_class, yend=max_class)) +
  geom_segment( mapping=aes(x=-0.95, xend=-0.97, y=(min_class + max_class)/2, yend=(min_class + max_class)/2)) +
  geom_text(aes(x = -1.11, y = (min_class + max_class)/2, label = "Land Class")) +
  
  theme(
    plot.margin = margin( l=70, t =20, b=20, r=20),
    
  ) +
  labs(title="Correlations with Land Cultivated (ha)",
           x ="Pearsons Correlation Coeff", y ="")

   # geom_segment( mapping=aes(x=min_year, xend=min_year, y=-4, yend=-3), color=pal_gf)+
   # geom_segment( mapping=aes(x=max_year, xend=max_year, y=-4, yend=-3), color=pal_gf)+

  
head(corr_matrix)

melted_cormat <- melt(cormat)
tidyr::pivot_longer(corr_matrix)

# pairs(indicator_data[c(x,y)])

# ggpairs(indicator_data[c(
#   y,
#   "population_density_mean_mean",
#   "accessibility_mean",
#   "b1_mean_mean"
# )])
# 
# ggpairs(indicator_data[c(x,y)]) 






# Plotting Distributions --------------------------------------------------



# ggplot() +
#   geom_sf(data=world_all)+
#   geom_sf(data=joined_df) 
# 
# plot(rhomis_data)

layers_to_plot <- c(x,"ADM0_NAME")[c(x,"ADM0_NAME") %in% colnames(fao_level_2_geo)]

map <- mapview(fao_level_2_geo[c("ADM0_NAME","population_density_mean_mean")], 
          alpha=1, 
          color="black", 
          # col.regions = brewer.pal(n = length(unique(fao_level_2[["ADM0_NAME"]])), name="Paired"),
          alpha.regions=0.8, 
          burst=T,
          hide=T) 

# map <-  mapview(rhomis_data, alpha=1, 
#                 color="white",  col.regions="black")

map
