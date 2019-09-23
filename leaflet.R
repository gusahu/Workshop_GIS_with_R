# Load the leaflet library
install.packages("leaflet")
library(leaflet)
library("tidyverse")

# Create a leaflet map with default map tile using addTiles()
leaflet() %>%
  addTiles()


# Print the providers list included in the leaflet library
providers

# Print only the names of the map tiles in the providers list 
names(providers)

# Use str_detect() to determine if the name of each provider tile contains the string "CartoDB"
str_detect(names(providers), "CartoDB")

# Use str_detect() to print only the provider tile names that include the string "CartoDB"
names(providers)[str_detect(names(providers), "CartoDB")]

# Change addTiles() to addProviderTiles() and set the provider argument to "CartoDB"
leaflet() %>% 
  addProviderTiles(provider = "CartoDB")

# Create a leaflet map that uses the Esri provider tile 
leaflet() %>% 
  addProviderTiles("Esri")

# Create a leaflet map that uses the CartoDB.PositronNoLabels provider tile
leaflet() %>% 
  addProviderTiles("CartoDB.PositronNoLabels")

# Map with CarDB tile centered on Cartagena with a zoom of 6

# Map with CartoDB tile centered on DataCamp's NYC office with ofom of  
leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = -75.5144424, lat = 10.3997202,  zoom = 18)

# Map with CartoDB.PositronNoLabels tile centered on DataCamp's Belgium office with zoom of 4
leaflet() %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  setView(lng = dc_hq$lon[2], lat = dc_hq$lat[2], zoom = 4) 

# A map with a narrowerview
leaflet(options = 
          leafletOptions(minZoom = 14, dragging = FALSE))  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = -75.5144475, lat = 10.3997856, zoom = 12) 

dc_hq <- 
  tibble(
    hq  = c("DataCamp - NYC","DataCamp -Belgium"),
    lon = c(-73.98575, 4.717863),
    lat = c(40.74856, 50.881363))

leaflet(options = leafletOptions(
  # Set minZoom and dragging 
  minZoom = 12, dragging = TRUE)) %>% 
  addProviderTiles("CartoDB") %>% 
  
  # Set default zoom level
  setView(lng = dc_hq$lon[2], lat = dc_hq$lat[2], zoom = 14) %>% 
  
  # Set max bounds of map
  setMaxBounds(lng1 = dc_hq$lon[2] + .05,
               lat1 = dc_hq$lat[2] + .05,
               lng2 = dc_hq$lon[2] - .05,
               lat2 = dc_hq$lat[2] - .05)

# Plot DataCamp's NYC HQ
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = dc_hq$lon[1], lat = dc_hq$lat[1])

# Plot DataCamp's NYC HQ with zoom of 12    
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = -73.98575, lat = 40.74856)  %>% 
  setView(lng = -73.98575, lat = 40.74856, zoom = 18)

# Plot both DataCamp's NYC and Belgium locations
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat)

#Adding Popups and Storing your Map To make our map more informative we can add popups.
#To add popups that appear when a marker is clicked we need to specify the popup argument in
#the addMarkers() function. Once we have a map we would like to preserve, we can store it 
#in an object. Then we can pipe this object into functions to add or edit the map's layers.

# Store leaflet hq map in an object called map
map <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # Use dc_hq to add the hq column as popups
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat,
             popup = dc_hq$hq)

# Center the view of map on the Belgium HQ with a zoom of 5
map_zoom <- map %>%
  setView(lat = 50.881363, lng = 4.717863,
          zoom = 3)

# Print map_zoom
map_zoom













