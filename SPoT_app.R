## To deploy to shinyapps.io, include:
##      - this script 
##      - all of the files in the 'data_processed' folder 
## (no other files need to be published - e.g., don't need to publish the 
## 'data_raw' folder)


# load packages -----------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(conflicted)
library(sf)
library(leaflegend)
library(leaflet)
library(glue)
library(janitor)
library(here)
library(FedData)
library(htmlwidgets)

## conflicts
conflicts_prefer(dplyr::filter)



# setup -------------------------------------------------------------------

## coordinate systems for transformations
projected_crs <- 3310 # see: https://epsg.io/3310 
# other options: 26910 see: https://epsg.io/26910
# resources: 
# https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=109326&inline
# 
geographic_crs <- 4269 # see: https://epsg.io/4269
# see: https://epsg.io/4326



# load data ----------------------------------------------------------------
## SPoT catchments ----
spot_catchments <- st_read(here('data_processed', 
                                'spot_catchments_2019-03-04.gpkg'))

## SPoT sites ----
spot_sites <- st_read(here('data_processed', 
                           'spot_sites_2022.gpkg')) %>% 
    mutate(location_color = as.factor(location_color))

## CalEnviroScreen 4 ----
ces_4 <- st_read(here('data_processed', 
                      'calenviroscreen_4-0.gpkg')) %>% 
    mutate(c_iscore = case_when(c_iscore == -999 ~ NA,
                                TRUE ~ c_iscore),
           c_iscore_p = case_when(c_iscore_p == -999 ~ NA,
                                  TRUE ~ c_iscore_p))

## tribal boundaries ----
tribal_bounds_bia <- st_read(here('data_processed', 
                                  'ca_tribal_boundaries_bia.gpkg'))

## NLCD ----
nlcd_legend <- nlcd_colors() %>% 
    filter(!str_detect(string = Description, pattern = 'Alaska only')) %>% 
    select(Class, Color)



# web services ------------------------------------------------------------
## NLCD (see: https://www.mrlc.gov/data-services-page AND more generally: https://www.mrlc.gov/data/nlcd-2019-land-cover-conus)
wms_nlcd <- 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/wms?service=WMS&'


# define UI ---------------------------------------------------------------
ui <- fillPage(
    # fillRow(
    leafletOutput("spot_map", height = "100%") %>% 
        # withSpinner(color="#0dc5c1") %>% # not working
        addSpinner(color = '#0dc5c1', 
                   spin = 'double-bounce' # 'fading-circle' 'rotating-plane'
        ) %>% 
        {.}
    # )
)



# Define server logic -----------------------------------------------------
server <- function(input, output) {
    
    ## create leaflet map ----
    output$spot_map <- renderLeaflet({
        
        ### create empty map ----
        leaflet_map <- leaflet()
        
        ### set initial zoom ----
        leaflet_map <- leaflet_map %>% 
            setView(lng = -119.5, # CA centroid: -119.5266
                    lat = 37.5, # CA centroid: 37.15246
                    zoom = 6) 
        
        ### add basemap options ----
        basemap_options <- c( # NOTE: use 'providers$' to see more options
            #'Stamen.TonerLite',
            'CartoDB.Positron',
            'Esri.WorldTopoMap', 
            # 'Esri.WorldGrayCanvas',
            'Esri.WorldImagery'#,
            # 'Esri.WorldStreetMap'
        ) 
        
        for (provider in basemap_options) {
            leaflet_map <- leaflet_map %>% 
                addProviderTiles(provider, 
                                 group = provider, 
                                 options = providerTileOptions(noWrap = TRUE))
        }
        
        ### add panes ----
        leaflet_map <- leaflet_map %>% 
            addMapPane('nlcd_pane', zIndex = 490) %>% 
            addMapPane('ces_4_pane', zIndex = 500) %>% 
            addMapPane('tribal_boundaries_pane', zIndex = 510) %>% 
            addMapPane('spot_catchments_pane', zIndex = 520) %>% 
            addMapPane('spot_sites_pane', zIndex = 530) %>%
            {.}
        
        
        #### add legend for Tribal Boundaries
        leaflet_map <- leaflet_map %>% 
            addLegend(position = 'bottomright', 
                      colors = 'blueviolet',
                      labels = 'Tribal Area',
                      opacity = 1, 
                      layerId = 'tribal_areas_legend', 
                      group = 'Tribal Areas')
        
        
        #### add legend for SPoT catchments ----
        leaflet_map <- leaflet_map %>% 
            addLegendSymbol(values = 'SPoT Catchment', 
                            color = 'dodgerblue', 
                            shape = 'line', 
                            strokeWidth = 2,
                            group = 'SPoT Catchments', 
                            position = 'bottomright')
        
        
        ### add SPoT sites ---- 
        
        #### add legend for SPoT sites shapes / borders
        leaflet_map <- leaflet_map %>% 
            addLegendSymbol(values = 'Possible SPoT Sample Location',
                            shape = 'circle',
                            fillColor = 'white',
                            width = 20, 
                            color = '#03F', 
                            strokeWidth = 8, 
                            opacity = 0.7, 
                            group = 'SPoT Sites', 
                            position = 'bottomright')
        
        #### Create color palette for SPoT toxicity scores
        site_colors <- c('red', 'orange', 'yellow', 'green', 'black')
        spot_sites_pal <- colorFactor(
            palette = site_colors, 
            domain = spot_sites$location_color, 
            levels = c(1, 2, 3, 4, 5)
        )
        
        #### add legend for SPoT colors
        leaflet_map <- leaflet_map %>% 
            addLegend(position = 'bottomright', # 'bottomright', 
                      colors = site_colors,
                      labels = c('0 - 80', '80 - 90', '90 - 95', '95 - 100+', 'DPR Locations'),
                      opacity = 1, 
                      layerId = 'sites_legend', 
                      group = 'SPoT Sites', 
                      title = paste0('SPoT Sites Avg Tox Response'))
        
        
        #### add SPoT sites
        leaflet_map <- leaflet_map %>%
            addCircleMarkers(data = spot_sites %>% 
                                 st_transform(crs = geographic_crs),
                             options = pathOptions(pane = 'spot_sites_pane'),
                             radius = 5,
                             stroke = TRUE, 
                             weight = ~ifelse(possible_sample_loc == 'yes',
                                              4, 
                                              1), 
                             color = ~ifelse(possible_sample_loc == 'yes',
                                             '#03F',
                                             'black'),
                             # color = "#03F", # 'black',
                             opacity = ~ifelse(possible_sample_loc == 'yes',
                                               0.8, # 1,
                                               0.6),
                             fill = TRUE, 
                             fillOpacity = 1, 
                             fillColor = ~ spot_sites_pal(location_color),
                             # highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE),
                             popup = ~paste0('<b>', '<u>', 'SPoT Site', '</u>', '</b>','<br/>',
                                             '<b>', 'Station Name: ', '</b>', station_name, '<br/>',
                                             '<b>', 'Station Code: ', '</b>', station_code, '<br/>',
                                             '<b>', 'Average Toxicity (2008-2022): ', '</b>', round(average_toxicity_2008_2022, 2), '<br/>',
                                             '<b>', 'Possible Sample Location: ', '</b>', possible_sample_loc, '<br/>'
                             ),
                             group = 'SPoT Sites', 
                             label = ~glue('SPoT Site (Station Name: {station_name})'))
        
        
        ### add SPoT catchments ----   
        leaflet_map <- leaflet_map %>%
            addPolylines(data = spot_catchments %>% 
                             st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet)
                         options = pathOptions(pane = 'spot_catchments_pane'),
                         color = 'dodgerblue', 
                         weight = 1.0,
                         smoothFactor = 1.0,
                         opacity = 0.7,
                         fill = FALSE,
                         fillOpacity = 0.4, 
                         fillColor = 'lightgrey',
                         highlightOptions = highlightOptions(color = "darkblue", 
                                                             weight = 2, 
                                                             bringToFront = TRUE),
                         popup = ~paste0('<b>', '<u>', 'SPoT Catchment', '</u>', '</b>','<br/>',
                                         '<b>', 'Station Name: ', '</b>', station_nam, '<br/>',
                                         '<b>', 'Station Code: ', '</b>', station_cod, '<br/>'#,
                         ),
                         group = 'SPoT Catchments',
                         label = ~glue('SPoT Catchment ({station_nam})')
            )
        
        
        ### add CalEnviroScreen ----
        
        #### Create color palette for toxicity scores ----
        ces_pal <- colorNumeric(
            palette = 'RdYlGn', 
            domain = ces_4$c_iscore_p, 
            reverse = TRUE)
        
        #### add CES polygons ----
        leaflet_map <- leaflet_map %>%
            addPolygons(data = ces_4 %>%
                            st_transform(crs = geographic_crs), # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]),
                        options = pathOptions(pane = "ces_4_pane"),
                        color = 'darkgrey', 
                        weight = 0.5,
                        smoothFactor = 1.0,
                        opacity = 0.8,
                        fillOpacity = 0.8,
                        # fillColor = ~colorNumeric(
                        #     palette = 'RdYlGn', 
                        #     domain = c_iscore_p, 
                        #     reverse = TRUE)(c_iscore_p),
                        # fillColor = ~colorNumeric('YlOrBr', c_iscore_p)(c_iscore_p), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                        fillColor = ~ces_pal(c_iscore_p), 
                        highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#,bringToFront = TRUE
                        popup = ~paste0('<b>', '<u>','CalEnviroScreen 4.0 (CES)', '</u>','</b>','<br/>',
                                        '<b>', 'Census Tract: ', '</b>',  tract, '<br/>',
                                        '<b>', 'CES Score: ', '</b>', round(c_iscore, 2), '<br/>',
                                        '<b>', 'CES Percentile: ', '</b>', round(c_iscore_p, 2), '<br/>'),
                        group = 'CalEnviroScreen 4.0'#,
                        # label = ~glue('CES 4.0 (Percentile: {round(c_iscore_p, 2)})')
            )
        
        #### add CES legend ----
        leaflet_map <- leaflet_map %>%
            addLegend(position = 'bottomleft', # 'bottomright',
                      pal = ces_pal,
                      values = ces_4$c_iscore_p,
                      opacity = 1,
                      layerId = 'ces_legend',
                      bins = 4,
                      group = 'CalEnviroScreen 4.0',
                      title = 'CalEnviroScreen 4.0 Percentile'
            )
        
        #### add tribal boundaries ----
        leaflet_map <- leaflet_map %>%
            addPolygons(data = tribal_bounds_bia %>%
                            st_transform(crs = geographic_crs), # ces3_poly %>% filter(California_County == cities_counties[[input$city_selected_1]]),
                        options = pathOptions(pane = "tribal_boundaries_pane"),
                        color = 'darkgrey', 
                        weight = 0.5,
                        smoothFactor = 1.0,
                        opacity = 0.8,
                        fillOpacity = 0.8, 
                        fillColor = 'blueviolet', 
                        highlightOptions = highlightOptions(color = "white", weight = 2), # fill = TRUE, fillColor = "white"),#,bringToFront = TRUE
                        popup = ~paste0('<b>', '<u>','Tribal Area', '</u>','</b>','<br/>',
                                        '<b>', 'Name: ', '</b>',  larname, '<br/>'#,
                                        # '<b>', 'Agency: ', '</b>', agency, '<br/>',
                        ),
                        group = 'Tribal Areas',
                        label = ~glue('Tribal Area ({larname})')
            )
        
        # #### add NLCD (land cover) ----
        # leaflet_map <- leaflet_map %>%
        #     addWMSTiles(
        #         wms_nlcd,
        #         layers = 'NLCD_2019_Land_Cover_L48',
        #         options = c(WMSTileOptions(format = 'image/png', transparent = TRUE),
        #                     pathOptions(pane = 'nlcd_pane')),
        #         attribution = 'National Land Cover Database 2019',
        #         group = 'Land Cover') %>% 
        #     hideGroup('Land Cover')
        # 
        # #### add NLCD legend ----
        # leaflet_map <- leaflet_map %>%
        #     addLegend(position = 'bottomleft', # 'bottomright',
        #               colors = nlcd_legend$Color,
        #               labels = nlcd_legend$Class,
        #               opacity = 1,
        #               layerId = 'nlcd_legend',
        #               group = 'Land Cover',
        #               title = 'Land Cover Classes (NLCD)') %>%
        #     hideGroup('Land Cover')
        
        
        ### add layer controls ----
        leaflet_map <- leaflet_map %>%
            addLayersControl(baseGroups = basemap_options,
                             overlayGroups = c(
                                 'CalEnviroScreen 4.0', 
                                 'SPoT Sites',
                                 'SPoT Catchments', 
                                 'Tribal Areas'#,
                                 #'Land Cover'#,
                                 #'Legend'
                             ),
                             options = layersControlOptions(collapsed = TRUE,
                                                            autoZIndex = TRUE))
        
        ## Try to make Land Use legend hidden by default (Need to add className arguments)
        ## See: https://github.com/rstudio/leaflet/issues/477
        # leaflet_map <- leaflet_map %>% 
        #     htmlwidgets::onRender(
        #         "function(el, x) {
        # var updateLegend = function () {
        # var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1).replace(/[^a-zA-Z]+/g, '');
        # document.querySelectorAll('.legend').forEach( a => a.hidden=true );
        # document.querySelectorAll('.legend').forEach( l => { if (l.classList.contains(selectedGroup)) l.hidden=false; } );
        # };
        # updateLegend();
        # this.on('baselayerchange', el => updateLegend());
        # }"
        #     )
    })
    
}


# run application  --------------------------------------------------------
shinyApp(ui = ui, server = server)
