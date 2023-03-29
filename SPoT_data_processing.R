
# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(here)
library(mapview)
library(readxl)
library(janitor)
library(httr)
library(esri2sf) # install using remotes::install_github("yonghah/esri2sf"); for more info see: https://github.com/yonghah/esri2sf 
library(tigris)
library(tools)

## conflicts ----
library(conflicted)
conflicts_prefer(dplyr::filter)


# SPoT Catchments ---------------------------------------------------------
## read spot catchments ----
spot_catchments <- st_read(here('data_raw', 
                                'SPoT_20190304_Catchments_WS.shp')) %>% 
    clean_names()
st_crs(spot_catchments)
glimpse(spot_catchments %>% st_drop_geometry())

# mapview(spot_catchments)

## write spot catchments ----
st_write(spot_catchments, 
         here('data_processed', 
              'spot_catchments_2019-03-04.gpkg'),
         append = FALSE)



# SPoT Sites (2022) -------------------------------------------------------

## read SPoT Sites ----
spot_sites_2022 <- read_excel(path = here('data_raw', 
                                          'SPoT 2022 sites.xlsx'), 
                              sheet = 'Site List',
                              skip = 2) %>% 
    clean_names() %>% 
    rename(id = x1)
glimpse(spot_sites_2022)

## clean SPoT Sites ----
### make sure all longitudes are negative ----
range(spot_sites_2022$target_long)
sum(spot_sites_2022$target_long > 0)
spot_sites_2022 <- spot_sites_2022 %>% 
    mutate(target_long = case_when(target_long > 0 ~ -1 * target_long,
                                   TRUE ~ target_long))
range(spot_sites_2022$target_long)
sum(spot_sites_2022$target_long > 0)

### convert avg tox to numeric ----
spot_sites_2022 <- spot_sites_2022 %>% 
    mutate(average_toxicity_2008_2022 = as.numeric(average_toxicity_2008_2022))

## convert SPoT Sites to sf ----
spot_sites_2022_sf <- st_as_sf(spot_sites_2022, 
                               coords = c('target_long', 'target_lat'),
                               crs = 4269) %>% 
    st_transform(crs = 3310)
st_crs(spot_sites_2022_sf)

### check SPoT Sites ----
plot(spot_catchments$geometry)
plot(spot_sites_2022_sf$geometry, add = TRUE, col = 'red')

## write SPoT Sites ----
st_write(spot_sites_2022_sf, 
         here('data_processed', 
              'spot_sites_2022.gpkg'), 
         append = FALSE)



# CES Data ----------------------------------------------------------------
## shapefile ----
temp_dir <- tempdir()
url_ces4_shp <- 'https://oehha.ca.gov/media/downloads/calenviroscreen/document/calenviroscreen40shpf2021shp.zip'

## download zip  file
GET(url = url_ces4_shp, 
    write_disk(file.path(temp_dir, 
                         basename(url_ces4_shp)),
               overwrite = TRUE))
unzip(zipfile = file.path(temp_dir, 
                          basename(url_ces4_shp)), 
      exdir = file.path(temp_dir, 
                        basename(url_ces4_shp) %>% 
                            file_path_sans_ext()))
# exdir = tempdir())
# unlink('calenviroscreen40shpf2021shp.zip')

sf_ces4 <- st_read(file.path(temp_dir, 
                             basename(url_ces4_shp) %>% 
                                 file_path_sans_ext())) %>% 
    arrange(Tract) %>% 
    clean_names()
# st_crs(sf_ces4)
names(sf_ces4)

## write CES data ----
st_write(sf_ces4, 
         here('data_processed', 
              'calenviroscreen_4-0.gpkg'), 
         append = FALSE)



# CA boundary -------------------------------------------------------------
## get CA boundary ----
ca_boundary <- states(year = 2020, 
                      cb = TRUE) %>% # use cb = TRUE to get the cartographic boundary file
    filter(STUSPS == 'CA') %>%
    st_transform(3310)

## write CA boundary ----
st_write(ca_boundary, 
         here('data_processed', 
              'ca_boundary.gpkg'), 
         append = FALSE)



# tribal boundaries -------------------------------------------------------
## from Bureau of Indian Affairs - see: https://biamaps.doi.gov/bogs/datadownload.html

## get boundaries (all US) ----
tribal_bounds_bia <- esri2sf(
    url = 'https://biamaps.doi.gov/server/rest/services/DivLTR/BIA_AIAN_National_LAR/MapServer/0',
    crs = NULL) %>%
    rename(geom = geoms) %>% 
    clean_names()
st_crs(tribal_bounds_bia)
tribal_bounds_bia <- tribal_bounds_bia %>% 
    st_transform(3310)

## filter for tribal areas in CA ----
tribal_bounds_bia <- tribal_bounds_bia %>%
    st_filter(ca_boundary)

## write tribal boundaries ----
st_write(tribal_bounds_bia,
         here('data_processed', 
              'ca_tribal_boundaries_bia.gpkg'), 
         append = FALSE)

