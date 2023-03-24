

# packages ----------------------------------------------------------------
library(FedData)
library(tidyverse)
library(sf)
library(conflicted)
library(here)
library(mapview)
library(readxl)
library(janitor)
library(httr)
library(esri2sf) # install using remotes::install_github("yonghah/esri2sf"); for more info see: https://github.com/yonghah/esri2sf 
library(tigris)
library(raster)
library(leaflet)


# conflicts ---------------------------------------------------------------
conflicts_prefer(dplyr::filter)




# NLCD Data ---------------------------------------------------------------

ca_counties <- counties(state = 'CA', 
                        year = 2019)
# counties_list <- c('Sacramento', 'Placer', 'El Dorado', 'Yolo')
# ca_counties <- ca_counties %>% filter(NAME %in% counties_list)

walk(.x = ca_counties$NAME,
     .f = ~ get_nlcd(
         template = ca_counties %>%
             filter(NAME ==.x),
         label = .x,
         year = 2019,
         dataset = 'landcover',
         extraction.dir = here('NLCD_data')
     ))


NLCD_Sac <- terra::rast(here('NLCD_data', 'Sacramento_NLCD_Land_Cover_2019.tif'))
# z <- terra::levels(NLCD_Sac)[[1]]
# View(z)
# values(NLCD_Sac)[1:100]
# 
# NLCD_Sac_rast <- raster::raster(here('NLCD_data', 'Sacramento_NLCD_Land_Cover_2019.tif'))
# zz <- raster::levels(NLCD_Sac_rast)[[1]]
# View(zz)
# values(NLCD_Sac_rast)[1:100]

NLCD_Pla <- terra::rast(here('NLCD_data', 'Placer_NLCD_Land_Cover_2019.tif'))
NLCD_ElD <- terra::rast(here('NLCD_data', 'El Dorado_NLCD_Land_Cover_2019.tif'))
NLCD_Yol <- terra::rast(here('NLCD_data', 'Yolo_NLCD_Land_Cover_2019.tif'))

NLCD_merge <- terra::merge(NLCD_Sac, NLCD_Pla)
NLCD_merge_2 <- terra::merge(NLCD_Sac, NLCD_Pla, NLCD_ElD, NLCD_Yol)

length(values(NLCD_merge))
length(values(NLCD_merge_2))

terra::plot(NLCD_merge_2)

## Sac NLCD ----
# sac_boundary <- ca_counties %>%
#     filter(NAME == 'Sacramento')
# 
# NLCD_Sac <- get_nlcd(
#     template = sac_boundary,
#     label = "Sac",
#     year = 2019,
#     dataset = 'landcover',
#     extraction.dir = here('NLCD_data_Sac')
# )

## CA NLCD ----
# ca_boundary <- states(year = 2019) %>% 
#     filter(STUSPS == 'CA') %>%
#     st_transform(3310)
# 
# NLCD_CA <- get_nlcd(
#     template = ca_boundary,
#     label = "CA",
#     year = 2019, 
#     dataset = 'landcover', 
#     extraction.dir = here('NLCD_data_CA')
# )

raster::plot(NLCD)
terra::plot(NLCD)
terra::polys(sac_boundary %>% st_transform(3310))
st_crs(NLCD)


# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(NLCD),
#                     na.color = "transparent")

## factor pallette
pal_fac <- colorFactor(
    c('darkblue', #10
                'red', 'red', 'red', 'red', # 20
                'grey', # 30
                'darkgreen', 'darkgreen', 'darkgreen', # 40
                'brown', # 50
                'lightgreen', # 70
                'gold', 'gold', # 80
                'lightblue', 'lightblue' # 90
    ), 
    as.factor(values(NLCD)), 
    na.color = "transparent"
)
z_fac <- leaflet() %>% 
    addTiles() %>%
    addRasterImage(NLCD, 
                   colors = pal_fac, 
                   opacity = 0.8) %>%
    addLegend(pal = pal_fac, 
              values = as.factor(values(NLCD)),
              title = "NLCD")


## bins ----
pal_bin <- colorBin(domain = values(NLCD), 
                    bins = c(0, 19, 29, 39, 49, 59, 79, 89, 100), 
                    palette = c('darkblue', #10
                                          'red', # 20
                                          'grey', # 30
                                          'darkgreen', # 40
                                          'brown', # 50
                                          'lightgreen', # 70
                                          'gold', # 80
                                          'lightblue' # 90
                    ),
                    na.color = "transparent"
)

z_bin <- leaflet() %>% 
    addTiles() %>%
    addRasterImage(NLCD, 
                   colors = pal_bin, 
                   opacity = 0.8) %>%
    addLegend(pal = pal_bin, 
              values = values(NLCD),
              title = "NLCD")
z_bin


### Aggregate
# https://stackoverflow.com/questions/32278825/how-to-change-the-resolution-of-a-raster-layer-in-r
res(NLCD)
NLCD_agg <- NLCD %>% raster::aggregate(fact = 4, 
                                       fun = median)
res(NLCD_agg)
z <- values(NLCD_agg)
z[1:100]
zz <- tibble(x = z)
raster::plot(NLCD_agg)
range(values(NLCD_agg))


pal_agg <- colorBin(domain = values(NLCD_agg), 
                    bins = c(10, 20, 30, 40, 50, 70, 80, 89, 100), 
                    palette = c('darkblue', # 10-20
                                          'red', # 20-30
                                          'grey', # 30-40
                                          'darkgreen', # 40-50
                                          'brown', # 50-70
                                          'lightgreen', # 70-80
                                          'gold', # 80 - 90
                                          'lightblue' # 90-100
                    ),
                    na.color = "transparent"
)


z_bin_agg <- leaflet() %>% 
    addTiles() %>%
    addRasterImage(NLCD_agg, 
                   colors = pal_agg, 
                   opacity = 0.8) %>%
    addLegend(pal = pal_agg, 
              values = values(NLCD_agg),
              title = "NLCD Aggregated", opacity = 1)
z_bin_agg



# reclassify --------------------------------------------------------------
# https://stackoverflow.com/questions/51092029/changing-values-in-a-raster
NLCD_val <- tibble(x = values(NLCD))
NLCD_val %>% count(x) %>% View('NLCD_val')
NLCD_reclass <- reclassify(NLCD, cbind(22, 21))
NLCD_val_reclass <- tibble(x = values(NLCD_reclass))
NLCD_val_reclass %>% count(x) %>% View('NLCD_reclass')
