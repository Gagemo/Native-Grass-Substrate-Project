################################################################################
################################################################################
######################### Grass-Substrate Project ##############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
#########################        Range Maps       ##############################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

##########################    Installs Packages   ##############################

list.of.packages <- c("tidyverse", "sp", "rgdal", "rgdal", "rgeos", "raster", 
                      "rgbif", "tmap", "rinat", "leaflet", "conflicted", "sf", 
                      "rangemap", "mapview", "htmlwidgets", "pacman")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################

library(pacman)
p_load(tidyverse, sp, rgdal, rgeos, raster, tmap, rinat, leaflet, conflicted, 
       sf, rangemap, rgbif, mapview, htmlwidgets)

################## Prevents Conflict with Functions   ##########################

conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("count", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("arrange", "dplyr", quiet = TRUE)

################## Reads in Voucher Data & FL County Map Data  #################

voucher = read.csv("02_Clean_Data/Grass Substrate Project - RangeMap.csv")
FL_sf <- 
  st_read("02_Clean_Data/geojson-fl-counties-fips.json", quiet = TRUE) %>% 
  st_transform(4326)

################ Changes Column Names to Help with Merge Later #################

FL_sf <- subset(FL_sf, select = -c(COUNTY)) 
colnames(FL_sf)[4] <- "County"

############# Creates a Data Frame from Voucher Data for each Species ##########

ind_voucher = dplyr::filter(voucher, Species == "Sorghastrum secundum") 
wire_voucher = dplyr::filter(voucher, Species == "Aristida stricta") 
sugar_voucher = dplyr::filter(voucher, Species == "Saccharum giganteum") 

################ Merge Voucher Data with County Map Data  ######################

FL_ind = merge(FL_sf, ind_voucher, by.x = "County")
FL_wire =  merge(FL_sf, wire_voucher, by.x = "County")
FL_sugar = merge(FL_sf, sugar_voucher, by.x = "County")

################# Creates Bounding Box to Pull iNat Data  ######################

FL_bb <- FL_sf %>% 
  st_bbox()

################ Downloads iNat data for each species ##########################
################ Checks & prevents re download #################################

search_S.secundum_fn <- "02_Clean_Data/S.secundum_FL.Rdata"
if (file.exists(search_S.secundum_fn)) {
  load(search_S.secundum_fn)
} else {  
  inat_S.secundum_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                     taxon_name = "Sorghastrum secundum",
                                     year = NULL,
                                     month = NULL,
                                     maxresults = 1000)
  save(inat_S.secundum_df, file = search_S.secundum_fn)
}

search_A.beyrichiana_fn <- "02_Clean_Data/A.beyrichiana_FL.Rdata"
if (file.exists(search_A.beyrichiana_fn)) {
  load(search_A.beyrichiana_fn)
} else {  
  inat_A.beyrichiana_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                        taxon_name = "Aristida beyrichiana",
                                        year = NULL,
                                        month = NULL,
                                        maxresults = 1000)
  save(inat_A.beyrichiana_df, file = search_A.beyrichiana_fn)
}

search_S.giganteum_fn <- "02_Clean_Data/S.giganteum_FL.Rdata"
if (file.exists(search_S.giganteum_fn)) {
  load(search_S.giganteum_fn)
} else {  
  inat_S.giganteum_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                      taxon_name = "Saccharum giganteum",
                                      year = NULL,
                                      month = NULL,
                                      maxresults = 1000)
  save(inat_S.giganteum_df, file = search_S.giganteum_fn)
}

################ Saves iNat Data into Data Frame  ##############################
################ Selects Key Columns & Combines with Map Data ##################

inat_S.secundum_fn = inat_S.secundum_df %>% 
  dplyr::select(longitude, latitude, datetime, common_name, scientific_name, 
                image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_A.beyrichiana_fn = inat_A.beyrichiana_df %>% 
  dplyr::select(longitude, latitude, datetime, common_name, scientific_name, 
                image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_S.giganteum_fn = inat_S.giganteum_df %>% 
  dplyr::select(longitude, latitude, datetime, common_name, scientific_name, 
                image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

################# Creates Leaflet Maps for each Species  #######################

S.secundum = leaflet(FL_ind, options = leafletOptions(zoomControl = TRUE)) %>%
  addMapPane("layer1", zIndex = 410) %>%  # Level 1: bottom
  addMapPane("layer2", zIndex = 420) %>%
  addMapPane("layer3", zIndex = 430) %>%
  addMapPane("layer4", zIndex = 440) %>% 
  addProviderTiles("GeoportailFrance.orthos", 
                   options = pathOptions(pane = "layer1")) %>% 
  addPolygons(fill = TRUE, fillColor = "forestgreen",color = "transparent", 
              weight = 1,fillOpacity = 0.4, 
              options = pathOptions(pane = "layer2")) %>%
  addPolylines(data = FL_sf, color = "black", weight = 1, opacity = 1.0,
               options = pathOptions(pane = "layer3")) %>%
  addCircleMarkers(data = inat_S.secundum_fn, popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "gold", opacity = 1,
                   fillOpacity = 1, color = "black", 
                   options = pathOptions(pane = "layer4")) %>%
  addLegend("bottomleft", 
            colors = c("forestgreen", "gold"),
            labels = 
              c("Florida Plant Atlas County Vouchers", "iNat Observations"),
            title = "Occurrence of Sorghastrum secundum in Florida",
            opacity = 1)
S.secundum

A.beryichiana = leaflet(FL_wire, 
                        options = leafletOptions(zoomControl = TRUE)) %>%
  addMapPane("layer1", zIndex = 410) %>%  # Level 1: bottom
  addMapPane("layer2", zIndex = 420) %>%
  addMapPane("layer3", zIndex = 430) %>%
  addMapPane("layer4", zIndex = 440) %>% 
  addProviderTiles("GeoportailFrance.orthos", 
                   options = pathOptions(pane = "layer1")) %>% 
  addPolygons(fill = TRUE, fillColor = "forestgreen",color = "transparent", 
              weight = 1,fillOpacity = 0.4, 
              options = pathOptions(pane = "layer2")) %>%
  addPolylines(data = FL_sf, color = "black", weight = 1, opacity = 1.0,
               options = pathOptions(pane = "layer3")) %>%
  addCircleMarkers(data = inat_A.beyrichiana_fn, 
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "gold", opacity = 1,
                   fillOpacity = 1, color = "black", 
                   options = pathOptions(pane = "layer4")) %>%
  addLegend("bottomleft", 
            colors = c("forestgreen", "gold"),
            labels = 
              c("Florida Plant Atlas County Vouchers", "iNat Observations"),
            title = "Occurrence of Aristida beyrichiana in Florida",
            opacity = 1)
A.beryichiana

S.giganteum =
  leaflet(FL_sugar, options = leafletOptions(zoomControl = TRUE)) %>%
  addMapPane("layer1", zIndex = 410) %>%  # Level 1: bottom
  addMapPane("layer2", zIndex = 420) %>%
  addMapPane("layer3", zIndex = 430) %>%
  addMapPane("layer4", zIndex = 440) %>% 
  addProviderTiles("GeoportailFrance.orthos", 
                   options = pathOptions(pane = "layer1")) %>% 
  addPolygons(fill = TRUE, fillColor = "forestgreen",color = "transparent", 
              weight = 1,fillOpacity = 0.4, 
              options = pathOptions(pane = "layer2")) %>%
  addPolylines(data = FL_sf, color = "black", weight = 1, opacity = 1.0,
               options = pathOptions(pane = "layer3")) %>%
  addCircleMarkers(data = inat_A.beyrichiana_fn, 
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "gold", opacity = 1,
                   fillOpacity = 1, color = "black", 
                   options = pathOptions(pane = "layer4")) %>%
  addLegend("bottomleft", 
            colors = c("forestgreen", "gold"),
            labels = 
              c("Florida Plant Atlas County Vouchers", "iNat Observations"),
            title = "Occurrence of Saccharum giganteum in Florida",
            opacity = 1)
S.giganteum

####################### Saves Leaflet Maps  ####################################

saveWidget(widget = S.secundum, file = "05_Figures/S.secundum.html",
           selfcontained = TRUE)
saveWidget(widget = A.beryichiana, file = "05_Figures/A.beryichiana.html",
           selfcontained = TRUE)
saveWidget(widget = S.giganteum, file = "05_Figures/S.giganteum.html",
           selfcontained = TRUE)

################################################################################
################################################################################
################################# END ##########################################
################################################################################
################################################################################
