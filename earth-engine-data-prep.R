#' Preparing and Mergining Datasets
#' 
#' This script merges household survey (RHoMIS)
#' with datasets from Google Earth Engine (GEE)
#' and the GAEZ v4 Data Portal 
#' 
#' The data from GEE was generated using scripts 
#' from the following repository:
#' 
#' https://github.com/l-gorman/earth-engine-farm-size-analysis
#' 
#' Data from GAEZ can be explored and downloaded here:
#' 
#' https://gaez.fao.org/pages/data-viewer
#' 
#' With more notes on Accessing GAEZ data here:
#' 
#' https://gaez.fao.org/pages/data-access-download
#' 
#' For more information, please see the README of this
#' repository
#' 
#' 
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tibble)
library(jsonlite)

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


# -------------------------------------------------------------------------------------------------------------
# Defining Functions -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------

#' Read and Transform EE DF
#'
#' Read and transform data downloaded
#' from google earth engine.
#' 
#'
#' @param file_name The name of the file to be downloaded (not the entire path)
#' @param basepath The folder where we find the file (relative to current working directory)
#' @param categorical Whether or not the variable is categorical or not
#' @param band_name If the variable is categorical, the name of the band you would like in your output
#'
#' @return
#' @export
#'
#' @examples
read_and_tranform_ee_df <- function(file_name,
                                    basepath = 'data/earth-engine/level-2-variables/',
                                    categorical=FALSE,
                                    band_name=NULL){
 
  df <- readr::read_csv(paste0(basepath,file_name))
  
  # Removing unnecessary column names
  if (".geo" %in% colnames(df)){
    df[".geo"] <- NULL
  }
  if ("system:index" %in% colnames(df)){
    df["system:index"] <- NULL
  }
  
  # In the case where we need to 
  # extract categorical band info
  if (categorical==T){
    freq_vector <- df$pixelFrequency
    
    
    freq_vector <- gsub("=", '":', freq_vector)
    freq_vector <- gsub("{", '{"', freq_vector, fixed=T)
    freq_vector <- gsub(", ", ',"', freq_vector, fixed=T)
    
    freq_df <- lapply(freq_vector, function(item){
      fromJSON(item) %>% tibble::as.tibble()
      
    }) %>% dplyr::bind_rows()
    
    if (all(!is.na(as.numeric(colnames(freq_df))))){
      sorted_colnames <- sort(as.numeric(colnames(freq_df)), index.return = T)
      sorted_index <- sorted_colnames$ix
      
      freq_df <- freq_df[sorted_index]
      
    }
    
    
    if (is.null(band_name)){
      stop("Need to supply a name for the band")
    }
    colnames(freq_df) <- paste0(band_name,"_",colnames(freq_df))
    df$pixelFrequency <- NULL
    df <- df %>% dplyr::bind_cols(freq_df)
    
  }
  
  return(df)
  
}


# -------------------------------------------------------------------------------------------------------------
# Reading in Data -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------

# RHoMIS Data
rhomis_data <- readr::read_csv("./data/rhomis-data/processed_data/processed_data.csv")
rhomis_data <- rhomis_data[!is.na(rhomis_data$gps_lat) & !is.na(rhomis_data$gps_lon),]
rhomis_data <- st_as_sf(rhomis_data, coords = c("gps_lon", "gps_lat"), 
                        crs = 4326, agr = "constant", remove = F)



# Earth Engine Data

# FAO administrative data
fao_level_2 <- geojson_sf('data/earth-engine/level-2-variables/fao-gaul-level-2.geojson')
fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
fao_level_2 <-st_set_crs(fao_level_2,'EPSG:4326')


# Elevation 
elevation_data <- read_and_tranform_ee_df("digital-elevation-zone-2.csv")

# Land Cover Categories (coverage)
land_cover_cat <- read_and_tranform_ee_df( "land-cover-categories-level-2.csv",
                                    categorical=T,
                                    band_name="land_cat")

# NDVI 
ndvi_data <- read_and_tranform_ee_df("ndvi-zone-2.csv")

# Night Lights 
night_lights_data <- read_and_tranform_ee_df("night-time-light-mean-zone-2.csv")

# Pop density 
pop_density_data <- read_and_tranform_ee_df("population-density-zone-2.csv")

# Topographic Diversity
topographic_diversity_data <- read_and_tranform_ee_df("topographic-diversity-zone-2.csv")

# Hospital Travel Time
travel_time_health_data <- read_and_tranform_ee_df("travel-time-to-health-zone-2.csv")




# Agro-Eco Zone Data (GAEZ)
aez_33_classes <- raster("./data/aez/gaez_v4_57_class/33_classes.tif")

aez_57_classes  <- raster("./data/aez/gaez_v4_57_class/57_classes.tif")
aez_57_classes <- projectRaster(aez_57_classes,aez_33_classes)

adjusted_length_growing_period  <- raster("./data/aez/gaez_v4_57_class/adjusted_length_growing_period.tif")
adjusted_length_growing_period <- projectRaster(adjusted_length_growing_period,aez_33_classes)

r_stack <- raster::stack(aez_33_classes,aez_57_classes,adjusted_length_growing_period)

points <- as(rhomis_data$geometry, Class="Spatial")

rasValue=extract(r_stack, points) %>% tibble::as_tibble()
colnames(rasValue) <- gsub("X33_classes", "AEZ_Classes_33", colnames(rasValue))
colnames(rasValue) <- gsub("X57_classes", "AEZ_Classes_57", colnames(rasValue))

# World Shapefile (Useful for plotting)
world_all <- readr::read_csv("./data/prepared-data/world-shapefile.csv")
world_all <- sf::st_as_sf(x = world_all, wkt = "geometry")
world_all <-st_set_crs(world_all,'EPSG:4326')


#### Joining data

# Joining earth engine data to administrative data

by_columns <- c("ADM0_CODE",
                "ADM1_CODE",
                "EXP2_YEAR",
                "ADM0_NAME",
                "ADM1_NAME",
                "ADM2_CODE",
                "ADM2_NAME",
                "DISP_AREA",
                "STATUS",
                "STR2_YEAR",
                "Shape_Area",
                "Shape_Leng")


fao_level_2 <-  fao_level_2 %>% merge(elevation_data, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)

fao_level_2 <-  fao_level_2 %>% merge(land_cover_cat, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)

fao_level_2 <-  fao_level_2 %>% merge(ndvi_data, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)

fao_level_2 <-  fao_level_2 %>% merge(night_lights_data, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)

fao_level_2 <-  fao_level_2 %>% merge(pop_density_data, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)

fao_level_2 <-  fao_level_2 %>% merge(topographic_diversity_data, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)

fao_level_2 <-  fao_level_2 %>% merge(travel_time_health_data, 
                                      by=by_columns, 
                                      all.x=T, 
                                      all.y=F)





joined_df <- st_join(x=rhomis_data, 
                     y=fao_level_2,
                     left=T)

joined_df <- joined_df %>% dplyr::bind_cols(rasValue)
joined_df <- joined_df[!is.na(joined_df$ADM0_CODE),]




columns_to_merge <- c("id_unique",colnames(fao_level_2),"AEZ_Classes_33", "AEZ_Classes_57","adjusted_length_growing_period", "gps_lon", "gps_lat", "geometry")
ind_data <- readr::read_csv("./data/rhomis-data/indicator_data/indicator_data.csv")
ind_data <- ind_data[ind_data$id_unique %in% joined_df$id_unique,]

ind_data <- ind_data %>% merge(joined_df[columns_to_merge], by="id_unique")




readr::write_csv(joined_df, "data/prepared-data/rhomis-ee-gaez.csv")
readr::write_csv(ind_data, "data/prepared-data/rhomis-indicator-ee-gaez.csv")



# readr::write_csv(joined_df, "data/prepared-data/rhomis-ee-gaez.csv")

if (file.exists("data/prepared-data/fao_level_2.geojson")){
  file.remove("data/prepared-data/fao_level_2.geojson")
}
st_write(fao_level_2, "data/prepared-data/fao_level_2.geojson")











