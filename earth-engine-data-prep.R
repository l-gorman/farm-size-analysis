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

library(ggplot2)
library(ggridges)
library(ggExtra)
library(RColorBrewer)



# Defining Functions

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

# Reading in Data ---------------------------------------------------------

# RHoMIS Data
rhomis_data <- readr::read_csv("./data/rhomis-data/processed_data/processed_data.csv")
rhomis_data <- rhomis_data[!is.na(rhomis_data$gps_lat) & !is.na(rhomis_data$gps_lon),]
rhomis_data <- st_as_sf(rhomis_data, coords = c("gps_lon", "gps_lat"), 
                        crs = 4326, agr = "constant")

# Earth Engine Data----

# FAO administrative data
fao_level_2 <- geojson_sf('data/earth-engine/level-2-variables/fao-gaul-level-1.geojson')
fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
fao_level_2 <-st_set_crs(fao_level_2,'EPSG:4326')

duplicates <- elevation_data[duplicated(elevation_data$ADM2_CODE) | duplicated(elevation_data$ADM2_CODE, fromLast = T),]

## Option tosimplify geometries if too large for plotting
# fao_level_2$geometry <- fao_level_2$geometry %>% 
#   sf::st_simplify(preserveTopology = FALSE, 
#                   dTolerance = 0.5)


# Elevation 
elevation_data <- read_and_tranform_ee_df("digital-elevation-zone-2.csv")

# Land Cover Categories (coverage)
land_cover_cat <- read_and_tranform_ee_df( "land-cover-categories-level-2.csv",
                                    categorical=T,
                                    band_name="land_cat")

# Agro-Eco Zone Data
aez_33_classes <- raster("./data/aez/gaez_v4_57_class/33_classes.tif")

aez_57_classes  <- raster("./data/aez/gaez_v4_57_class/57_classes.tif")
# Recast to original projection
aez_57_classes <- projectRaster(aez_57_classes,aez_33_classes)
r_stack <- raster::stack(aez_33_classes,aez_57_classes)

points <- as(rhomis_data$geometry, Class="Spatial")
rasValue=extract(r_stack, points) %>% tibble::as_tibble()

table(rasValue$X33_classes)
table(rasValue$X57_classes)



plot(aez_classes)

# World Shapefile (Useful for plotting)
world_all <- readr::read_csv("./data/prepared-data/world-shapefile.csv")
world_all <- sf::st_as_sf(x = world_all, wkt = "geometry")
world_all <-st_set_crs(world_all,'EPSG:4326')




#### Joining data

# Joining earth engine data to administrative data

temp <-  merge(fao_level_2,elevation_data, 
               by=c("ADM0_CODE",
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
                    "Shape_Leng" ), all.x=T, all.y=F)



joined_df <- st_join(x=rhomis_data, 
                     y=fao_level_2,
                     left=T)
# Subsetting only datasets which fall with the 
# regions specified
joined_df <- joined_df[!is.na(joined_df$ADM0_CODE),]










ggplot() +
  geom_sf(data=world_all)+
  geom_sf(data=joined_df) 

plot(rhomis_data)

map <-  mapview(rhomis_data$geometry, alpha=1, 
                color="white",  col.regions="black", burst=T) +
  mapview(fao_level_2["ADM0_NAME"], 
          alpha=1, 
          color="black", 
          col.regions = brewer.pal(n = length(unique(fao_level_2[["ADM0_NAME"]])), name="Paired"),
          alpha.regions=0.5, burst=T) 

map <-  mapview(rhomis_data, alpha=1, 
                color="white",  col.regions="black")

map
