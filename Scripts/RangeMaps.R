cat("\014") # Clears history

list.of.packages <- c("tidyverse", "sp", "rgdal", "rgdal", "rgeos", "raster", "rgbif",
                      "tmap", "rinat", "leaflet", "conflicted", "sf", "ggmap", "rangemap", "mapview")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library(rinat)
library(leaflet)
library(conflicted)
library(sf)
library(ggmap)
library(rangemap)
library(rgbif)
library(mapview)

#conflict_prefer("filter", "dplyr", quiet = TRUE)
#conflict_prefer("count", "dplyr", quiet = TRUE)
#conflict_prefer("select", "dplyr", quiet = TRUE)
#conflict_prefer("arrange", "dplyr", quiet = TRUE)

voucher = read.csv("Grass Substrate Project - RangeMap.csv")


# Now we can import the park boundary which is saved as a geojson file. We use 
# the sf package to import and transform the boundary to geographic coordinates, 
# and leaflet to plot it:
FL_sf <- st_read("geojson-fl-counties-fips.json", quiet = TRUE) %>% 
  st_transform(4326)
leaflet(FL_sf) %>% 
  addTiles() %>% 
  addPolygons()

FL_sf <- subset(FL_sf, select = -c(COUNTY))
FL_sf <- subset(FL_sf, select = -c(COUNTY))
colnames(FL_sf)[4] <- "County"

wire_voucher = dplyr::filter(voucher, Species == "Aristida stricta")

FL_wire = merge(FL_sf, wire_voucher, by.x = "County")


# Next, we get the bounding box of the study area, so we can pass it to the 
# iNaturalist API to tell it which area we’re interested in:
FL_bb <- FL_sf %>% 
  st_bbox()

# We’ll use the bounds argument in get_inat_obs() to specify the extent of our 
# search. bounds takes 4 coordinates. 
# The code below first checks if the data have 
# already been downloaded, to prevent downloading data twice.

# Sorghastrum secundum

search_S.secundum_fn <- "S.secundum_FL.Rdata"
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

inat_S.secundum_pcsp_popup_sf <- inat_S.secundum_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

#Maps

htmltools::p("Sorghastrum secundum in Florida", 
             style = "font-weight:bold; font-size:110%;")

leaflet(FL_wire, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("Esri.WorldPhysical") %>% 
  addPolygons(color = "green", weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>% 
  addCircleMarkers(data = inat_S.giganteum_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 2, weight = 4, opacity = 1, 
                   fillOpacity = 1, color = "grey") %>%
  addLegend("bottomright", 
            colors = c("green"),
            labels = c("Sorghastrum secundum"),
            title = "iNaturalist Research-grade Observations",
            opacity = 1)

ggplot() +             
  geom_sf(data = FL_sf, fill = "#C3D7A4") +
  geom_sf(data = inat_S.secundum_pcsp_popup_sf, color = "chartreuse")+                                
  theme_void() +                               
  theme(plot.background= element_blank(),     
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank()) 

# Aristida beyrichiana

search_A.beyrichiana_fn <- "A.beyrichiana_FL.Rdata"
if (file.exists(search_A.beyrichiana_fn)) {
  load(search_A.beyrichiana_fn)
} else {  
  inat_A.beyrichiana_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                     taxon_name = "Aristida beyrichiana",
                                     year = NULL,
                                     month = NULL,
                                     maxresults = 1000)
  save(inat_A.beyrichiana_df, file = search_S.secundum_fn)
}

inat_A.beyrichiana_pcsp_popup_sf <- inat_A.beyrichiana_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

#Maps

htmltools::p("Aristida beyrichiana in Florida", 
             style = "font-weight:bold; font-size:110%;")

leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("Esri.WorldPhysical") %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>% 
  addCircleMarkers(data = inat_A.beyrichiana_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "green") %>%
  addLegend("bottomright", 
            colors = c("green"),
            labels = c("Aristida beyrichiana"),
            title = "Aristida beyrichiana",
            opacity = 1)


ggplot() +             
  geom_sf(data = FL_sf, fill = "#C3D7A4") +
  geom_sf(data = inat_A.beyrichiana_pcsp_popup_sf, color = "blue")+                                
  theme_void() +                               
  theme(plot.background= element_blank(),     
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank()) 

# Saccharum giganteum
search_S.giganteum_fn <- "S.giganteum_FL.Rdata"
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

inat_S.giganteum_pcsp_popup_sf <- inat_S.giganteum_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

#Maps

htmltools::p("Saccharum giganteum in Florida", 
             style = "font-weight:bold; font-size:110%;")

m = leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("Esri.WorldPhysical") %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>% 
  addCircleMarkers(data = inat_S.giganteum_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 2, weight = 4, opacity = 1, 
                   fillOpacity = 1, color = "royalblue") %>%
  addLegend("bottomright", 
            colors = c("royalblue"),
            labels = c("Saccharum giganteum"),
            title = "iNaturalist Research-Grade Observations",
            opacity = 1)


mapshot(m, url = paste0(getwd(), "/map.html"),
        file = paste0(getwd(), "/map.png"))
htmlwidgets::saveWidget(m, "map.html", selfcontained = T)

ggplot() +             
  geom_sf(data = FL_sf, fill = "#C3D7A4") +
  geom_sf(data = inat_S.giganteum_pcsp_popup_sf, color = "royalblue")+                                
  theme_void() +                               
  theme(plot.background= element_blank(),     
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank())



htmltools::p("d", 
             style = "font-weight:bold; font-size:110%;")

leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("Esri.WorldPhysical") %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>% 
  addCircleMarkers(data = inat_S.secundum_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 2, weight = 4, opacity = 1, 
                   fillOpacity = 1, color = "chartreuse") %>%
  addCircleMarkers(data = inat_A.beyrichiana_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 2, weight = 4, opacity = 1, 
                   fillOpacity = 1, color = "goldenrod") %>%
  addCircleMarkers(data = inat_S.giganteum_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 2, weight = 4, opacity = 1, 
                   fillOpacity = 1, color = "royalblue") %>%
  addLegend("bottomright", 
            colors = c("chartreuse"),
            labels = c("Sorghastrum secundum"),
            title = "iNaturalist Research-grade Observations",
            opacity = 1)









