library(brms)
library(rstan)
library(jsonlite)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(sf)
library(sp)
library(stars)
library(FactoMineR)
library(factoextra)
library(lme4)
library(mgcv)
library(lqmm)
library(corrplot)
library(quantreg)
library(bamlss)
# Setting the mac Graphics ttype 
X11(type = "cairo")
# options(device = "X11")




# Read in Data ------------------------------------------------------------



# Reading in RHoMIS Dataset 
# This RHoMIS dataset has been 
# linked with the IPUMS 
# population level dataset
# GPS indicators rounded
# to preserve privacy
rhomis_df <- readr::read_csv("./data/prepared-data/rhomis-geo-data.csv")


# IPUMS GEO2 level dataset
ipums_df <- readr::read_csv("./data/prepared-data/ipums-all.csv")

# lsms_all <- readr::read_csv("./data/prepared-data/lsms-all.csv")


variable_categories <- jsonlite::read_json("./data/variable-categorisation.json")
# Creating GEO Datasets ---------------------------------------------------
rhomis_geo_data <- sf::st_as_sf(x = rhomis_df, wkt = "geometry")
rhomis_geo_data <- rhomis_geo_data[rhomis_geo_data$iso_3!="NER",]

ipums_geo <- sf::st_as_sf(x = ipums_df, wkt = "geometry")
ipums_geo <- ipums_geo[ipums_geo$iso_3!="NER",]

world_all <- readr::read_csv("./data/prepared-data/world-shapefile.csv")
world_all <- sf::st_as_sf(x = world_all, wkt = "geometry")

# lsms_all <- sf::st_as_sf(x = lsms_all, wkt = "geometry")


# Computing Indicators ----------------------------------------------------


# RHoMIS Income Columns
# Converting income columns
# from local currency unit (LCU)
# to PPP (internationally comparable)
income_columns_lcu <- c(
  "crop_income_lcu_per_year",
  "livestock_income_lcu_per_year",
  "total_income_lcu_per_year",
  "off_farm_income_lcu_per_year",
  "value_crop_consumed_lcu_per_hh_per_year",
  "value_livestock_products_consumed_lcu_per_hh_per_year",
  "value_farm_products_consumed_lcu_per_hh_per_year"
)
income_columns_ppp <- gsub("lcu", "ppp", income_columns_lcu)
rhomis_df[income_columns_ppp] <- rhomis_df[income_columns_lcu]/rhomis_df$currency_conversion_lcu_to_ppp


# Population columns came in form of totals
# Converte these into percentages for comparison

# between locations
population_columns_percentage <- paste0(
  variable_categories$ipums$population_columns,
  "_perc"
)

ipums_df[population_columns_percentage] <- 100*ipums_df[unlist(variable_categories$ipums$population_columns)]/ipums_df$TOTPOP_GEO2A

# Data Coverage and Completeness ------------------------------------------------

# Plotting points on map
bounding_box <- st_bbox(ipums_geo)

# Plots
subnational_boundary_plot <- ggplot() +
  geom_sf(data=world_all, size=0.5) +
  geom_sf(data=ipums_geo, color="black", size=0.1,aes(fill=iso_3))  +
  geom_sf(data=rhomis_geo_data, size=0.1) +
  coord_sf(xlim = c(bounding_box$xmin, bounding_box$xmax), ylim = c(bounding_box$ymin, bounding_box$ymax))

dir.create("./outputs/r_outputs/exploratory/",showWarnings =F ,recursive = T)
ggsave("./outputs/r_outputs/exploratory/subnational_boundaries.png", subnational_boundary_plot)




# Correlations and Distributions ------------------------------------------------










correlation_matrix <- cor(ipums_df[c(landscape_variables)], use = "complete.obs")




ipums.pca.res <- FactoMineR::PCA(ipums_df[c(
    landscape_variables,
    economic_subnational_indicators,
    employment_columns,
    population_columns
)], graph=T)



# Exploring RHoMIS Land Size Distribtions by are

colnames(rhomis_df)

table(rhomis_df$GEO2LABEL, rhomis_df$iso_country_code)


land_size_quantiles <- quantile(rhomis_df$land_cultivated_ha, probs = seq(0, 1, 0.01), na.rm=T, names=F)
quantile_99th <- land_size_quantiles[100]



rhomis_df[rhomis_df$land_quantile_99th,] %>% 
  group_by(iso_country_code) 
  
rhomis_df %>% 
  dplyr::filter(land_cultivated_ha < quantile_99th) %>% 
ggplot(aes(x=land_cultivated_ha))+
    geom_histogram() + facet_wrap("iso_country_code")


rhomis_df %>% 
  dplyr::filter(land_cultivated_ha < quantile_99th) %>% 
  ggplot(aes(x=land_cultivated_ha))+
  geom_boxplot() + facet_wrap("iso_country_code")



rhomis_df %>% 
  dplyr::filter(land_cultivated_ha < quantile_99th) %>% 
  ggplot(aes(x=land_cultivated_ha))+
  geom_histogram() + facet_wrap("GEO2")


rhomis_df %>% 
  dplyr::filter(land_cultivated_ha < quantile_99th) %>% 
  ggplot(aes(x=land_cultivated_ha))+
  geom_boxplot() + facet_wrap("GEO2")

rhomis_df %>% 
  group_by(GEO2) %>% 
  summarise("land_cult_mean" = mean(land_cultivated_ha, na.rm=T))


number_of_areas <- length(unique(rhomis_df$GEO2))
table(rhomis_df$GEO2)[]

corrplot(correlation_matrix, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)


#### Burkina Data

burkina_rhomis <- rhomis_df %>% filter(iso_country_code=="BF")
burkina_ipums <- ipums_df %>%  filter(iso_2=="BF")


N_rhomis <- nrow(burkina_rhomis)
N_ipums <- nrow(burkina_ipums)

burkina_rhomis %>% 
  group_by(GEO2LABEL) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=n)) + geom_histogram()

burkina_rhomis %>% 
  ggplot(aes(x=land_cultivated_ha)) + 
  geom_histogram() + 
  facet_wrap('GEO2')

burkina_rhomis %>% 
  ggplot(aes(x=land_cultivated_ha)) + 
  geom_boxplot() + 
  facet_wrap('GEO2')


numeric_indicators <- c(
  "land_cultivated_ha",
  "crop_income_ppp_per_year",
  "livestock_income_ppp_per_year",
  "value_crop_consumed_ppp_per_hh_per_year",
  "value_livestock_products_consumed_ppp_per_hh_per_year",
  "value_farm_products_consumed_ppp_per_hh_per_year",
  "hh_size_mae",
  "livestock_tlu"
)


f <- list(
  land_cultivated_ha ~ memployment + urban + gender + meducation +
    s(mbmi) + s(agechild) + s(district, bs = "mrf", xt = list("penalty" = K)) +
    s(district, bs = "re"),
  sigma  ~ memployment + urban + gender + meducation +
    s(mbmi) + s(agechild) + s(district, bs = "mrf", xt = list("penalty" = K)) +
    s(district, bs = "re")
)



burkina_rhomis


pairs(burkina_rhomis[numeric_indicators], col="blue", main="Scatterplots")

X <- names(which(colSums(!is.na(burkina_rhomis[landscape_variables]))>0))
X <- burkina_rhomis[X]
unlist(X)
pairs(X, col="blue", main="Scatterplots")

Y <-   "land_cultivated_ha"                  
X <- names(which(colSums(!is.na(burkina_rhomis[landscape_variables]))>0))
X <- paste0(X, collapse=" + ")

as.formula(X)

formula(land_cultivated_ha~X)
lm <- lm(
  formula=land_cultivated_ha ~ ., 
  data=burkina_rhomis[c("land_cultivated_ha","IGBP_OPENSHRBLND_percent_area_bin_GEO2IGBP_2012_2012")],
  )

summary(lm)

qreg <- rq(
  formula=land_cultivated_ha ~ ., 
  tau=seq(0.1,0.9,0.01), 
  data=burkina_rhomis[c(
    "land_cultivated_ha",
    "IGBP_OPENSHRBLND_percent_area_bin_GEO2IGBP_2012_2012",
    "IGBP_CROPLAND_percent_area_bin_GEO2IGBP_2012_2012",
    "ELECTRICITY_GEO2A"
    )],
  
 )


fit1 <- brm(bf(land_cultivated_ha ~ GEO2LABEL, sigma ~ GEO2LABEL),
            data = burkina_rhomis, family = gaussian())

summary(fit1)
plot(fit1, N = 2, ask = FALSE)

plot(conditional_effects(fit1), points = TRUE)
plot(conditional_effects(fit1), points = FALSE)


model <- lmer(land_cultivated_ha ~ hh_size_mae + (1 | GEO2) + (1|ELECTRICITY_GEO2A), 
            data=burkina_rhomis[c(
                                  "land_cultivated_ha",
                                  "hh_size_mae",
                                  "GEO2",
                                  "ELECTRICITY_GEO2A"
)])

summary(model)



summary(qreg)
plot(summary(qreg))
 
qreg_formula <- Y ~ X

lapply(X, function(x)
  {
  as.formula(x)
  })

complete.cases(burkina_rhomis[qreg_formula])

qreg <- rq(
  formula=qreg_formula, 
  tau=0.25, 
  data=burkina_rhomis,
  na.omit)





dat_smooth <- mgcv::gamSim(eg = 6, n = 200, scale = 2, verbose = FALSE)



rq()

