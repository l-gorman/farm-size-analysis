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
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(GGally)

library(moments) # Package for skewness
library(gamlss) # Gaussian additive models for location, scale, and shape
library(bamlss) # Bayesian additive models for location, scale, and shape
library(brms) # General package for bayesian models with lme4 syntax and stan backend
library(lme4) # General package for bayesian multi-level modelling.
library(FactoMineR) # Package for pca
library(factoextra) # Extra pca features

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

fao_level_2 <- tibble::as_tibble(fao_level_2)
  
fao_level_2$geo_id <- paste0(fao_level_2$ADM0_CODE, "_", 
                             fao_level_2$ADM1_CODE, "_",
                             fao_level_2$ADM2_CODE)

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

fao_level_2[land_cat_columns] <- lapply(fao_level_2[land_cat_columns] , function(column){
  column <- as.numeric(column)
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/fao_level_2$pixelCount
  return(column)
}) %>% dplyr::bind_cols()


new_land_cat_columns <- land_categories$Tag
colnames(indicator_data)[colnames(indicator_data) %in% land_cat_columns] <- new_land_cat_columns
colnames(fao_level_2)[colnames(fao_level_2) %in% land_cat_columns] <- new_land_cat_columns





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

colnames(fao_level_2)[colnames(fao_level_2) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(fao_level_2)[colnames(fao_level_2) == "b1_mean_mean"] <- "nightlights"
colnames(fao_level_2)[colnames(fao_level_2) == "population_density_mean_mean"] <- "population_density"

colnames(fao_level_2)[colnames(fao_level_2) == "elevation_mean"] <- "elevation"
colnames(fao_level_2)[colnames(fao_level_2) == "NDVI_mean_mean"] <- "ndvi"
colnames(fao_level_2)[colnames(fao_level_2) == "constant_mean"] <- "topographic_diversity"

indicator_data$geo_id <- paste0(indicator_data$ADM0_CODE, "_", 
                                indicator_data$ADM1_CODE, "_",
                                indicator_data$ADM2_CODE)




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

# ggplot(data = corr_matrix, aes(x=var1, y=var2, fill=value)) + 
#   geom_tile() +
#   theme_minimal()+ 
#   theme(axis.text.x = element_text(angle = 90, vjust = 1, 
#                                     hjust = 1))

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

aez <-  c("AEZ_Classes_33",
          "AEZ_Classes_57")

land_cat <- new_land_cat_columns

land_cult_corr$var_group <- NA
land_cult_corr$var_group[land_cult_corr$var2 %in% socio_economic] <- "Socio-Economic"
land_cult_corr$var_group[land_cult_corr$var2 %in% environmental] <- "Environmental"
land_cult_corr$var_group[land_cult_corr$var2 %in% land_cat] <- "Land-Cover-Class"
land_cult_corr$var_group[land_cult_corr$var2 %in% aez] <- "AEZ"



# min_social <- min(land_cult_corr$factor_level[land_cult_corr$var2 %in% socio_economic]) -0.5
# max_social <- max(land_cult_corr$factor_level[land_cult_corr$var2 %in% socio_economic]) + 0.5
# 
# min_env <- min(land_cult_corr$factor_level[land_cult_corr$var2 %in% environmental]) -0.5
# max_env <- max(land_cult_corr$factor_level[land_cult_corr$var2 %in% environmental]) + 0.5
# 
# min_class <- min(land_cult_corr$factor_level[land_cult_corr$var2 %in% land_cat]) -0.5
# max_class <- max(land_cult_corr$factor_level[land_cult_corr$var2 %in% land_cat]) + 0.5

ggplot(data = land_cult_corr, aes(y=var2, x=value)) + 
  geom_segment( aes(x=0, xend=value, y=var2, yend=var2), color="black")+
  geom_point( size=2, aes(x=value, color=var_group)) +
  coord_cartesian(xlim = c(-0.3, 0.3), # This focuses the x-axis on the range of interest
                  # ylim = c(-0.2, length(land_cult_corr$factor_level)),
                  clip = 'off')+
  # geom_segment( mapping=aes(x=-0.93, xend=-0.93, y=min_social, yend=max_social,)) + 
  # geom_segment( mapping=aes(x=-0.93, xend=-0.91, y=min_social, yend=min_social)) + 
  # geom_segment( mapping=aes(x=-0.93, xend=-0.91, y=max_social, yend=max_social)) + 
  # geom_segment( mapping=aes(x=-0.93, xend=-0.91, y=max_social, yend=max_social)) + 
  # geom_segment( mapping=aes(x=-0.93, xend=-0.95, y=(max_social + min_social)/2, yend=(max_social + min_social)/2)) +
  # geom_text(aes(x = -1.02, y = (max_social + min_social)/2, label = "Social")) +
  # 
  # geom_segment( mapping=aes(x=-1.1, xend=-1.1, y=min_env, yend=max_env)) + 
  # geom_segment( mapping=aes(x=-1.1, xend=-1.08, y=min_env, yend=min_env)) + 
  # geom_segment( mapping=aes(x=-1.1, xend=-1.08, y=max_env, yend=max_env)) + 
  # geom_segment( mapping=aes(x=-1.1, xend=-1.12, y=(max_env + min_env)/2, yend=(max_env + min_env)/2)) +
# geom_text(aes(x = -1.27, y = (min_env + max_env)/2, label = "Environmental")) +
# 
# 
# geom_segment( mapping=aes(x=-0.95, xend=-0.95, y=min_class, yend=max_class)) + 
# geom_segment( mapping=aes(x=-0.95, xend=-0.93, y=min_class, yend=min_class)) + 
# geom_segment( mapping=aes(x=-0.95, xend=-0.93, y=max_class, yend=max_class)) +
# geom_segment( mapping=aes(x=-0.95, xend=-0.97, y=(min_class + max_class)/2, yend=(min_class + max_class)/2)) +
# geom_text(aes(x = -1.11, y = (min_class + max_class)/2, label = "Land Class")) +
# 
# theme(
#   plot.margin = margin( l=70, t =20, b=20, r=20),
#   
# ) +
labs(title="Correlations with Land Cultivated (ha)",
     x ="Pearsons Correlation Coeff", y ="Landscape predictiors",
     color="Variable Class",
     caption = "\nLandscape variables sourced from GAEZ v4 and Google Earth Engine.
     Landscape variables aggregated to FAO GAUL level 2.
     Correlations represent correlation between household level land cultivated (ha)
     and aggregated landscape variable.
     **Outliers have been removed using 99th percentile (>24ha)
     ") 





# head(corr_matrix)
# 
# melted_cormat <- melt(cormat)
# tidyr::pivot_longer(corr_matrix)

# pairs(indicator_data[c(x,y)])

# ggpairs(indicator_data[c(
#   y,
#   "population_density_mean_mean",
#   "accessibility_mean",
#   "b1_mean_mean"
# )])
# 
# ggpairs(indicator_data[c(x,y)]) 



# Looking at Land Cult Stats ----------------------------------------------

land_cult_summary <- indicator_data %>% 
  dplyr::group_by(
    geo_id
  ) %>% 
  summarise(
    land_cult_mean = mean(land_cultivated_ha, na.rm=T),
    land_cult_stdev = sd(land_cultivated_ha, na.rm=T),
    land_cult_med = median(land_cultivated_ha, na.rm=T),
    land_cult_iqr = IQR(land_cultivated_ha, na.rm=T),
    land_cult_q_0.25 = quantile(land_cultivated_ha, probs=c(0.25),na.rm=T),
    land_cult_q_0.75 = quantile(land_cultivated_ha, probs=c(0.75),na.rm=T),
    land_cult_skew = moments::skewness(land_cultivated_ha, na.rm=T),
    land_cult_kurtosis = moments::kurtosis(land_cultivated_ha, na.rm=T)
  )


fao_level_2_land_cult <- merge(land_cult_summary,
                               fao_level_2, 
                               by="geo_id",
                               all.x=T,
                               all.y=F) %>% tibble::as_tibble()

colSums(is.na(fao_level_2))

y_subn <- c(
  "land_cult_mean",
  "land_cult_stdev",
  "land_cult_med",
  "land_cult_iqr",
  "land_cult_q_0.25",
  "land_cult_q_0.75"
)
x_subn <- x[x %in% colnames(fao_level_2_land_cult)]


fao_level_2_land_cult[c(x_subn,y_subn)] <- lapply(fao_level_2_land_cult[c(x_subn,y_subn)], 
                                                  function(x){
                                                    as.numeric(x)
                                                  }) %>% dplyr::bind_cols()

colSums(is.na(fao_level_2_land_cult))

corr_matrix <- round(cor(fao_level_2_land_cult[c(x_subn,y_subn)]),2) %>% tibble::as_tibble()
corr_matrix$var <- colnames(corr_matrix)
corr_matrix <- corr_matrix %>% pivot_longer(cols = colnames(corr_matrix)[colnames(corr_matrix)!="var"])

colnames(corr_matrix) <- c("var1", "var2", "value")

corr_matrix <- corr_matrix[corr_matrix$var1 %in% x_subn==F,]
corr_matrix <- corr_matrix[corr_matrix$var2 %in% y_subn==F,]

ggplot(data = corr_matrix, aes(x=var2, y=var1, fill=value)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1,
                                    hjust = 1))


# Reducing layers

FactoMineR::PCA(fao_level_2_land_cult[x_subn], scale.unit=TRUE, ncp=5, graph=T)

table(indicator_data$AEZ_Classes_33)
table(indicator_data$AEZ_Classes_57)

# Plotting Distributions --------------------------------------------------

# Model Building

library("gamlss")

gaml_fit <- gamlss(data = indicator_data[c(x,y)], 
                   formula = land_cultivated_ha ~ elevation + water_bodies + croplan + ndvi + nightlights + population_density,
                   sigma.formula = ~ elevation + water_bodies + croplan + ndvi + nightlights + population_density,
                   nu.formula = ~elevation + water_bodies + croplan + ndvi + nightlights + population_density,
                   family=LNO())


brm(formula = land_cultivated_ha  ~ elevation  + water_bodies + croplan + (1|geo_id), 
    data = indicator_data, family = lognormal(),
    prior = c(set_prior("normal(0,5)", class = "b"),
              set_prior("cauchy(0,2)", class = "sd")),
    warmup = 1000, iter = 2000, chains = 4,
    control = list(adapt_delta = 0.95))

# To fit a distributions we can use optim, R's built in optimiser:
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optim
# And choose to optimise using MLE, see: https://stats.stackexchange.com/questions/112451/maximum-likelihood-estimation-mle-in-layman-terms

# Train

# Validate

# Split


  






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


