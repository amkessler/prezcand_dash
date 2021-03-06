library(tidyverse)
library(lubridate)
library(janitor)
library(googlesheets)
library(leaflet)
library(ggmap)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(tigris)

#this will trigger a web page to authenticate with google account
# gs_ls() %>% View()


#register DW's 2020 google sheet
mykey <- Sys.getenv("DW2020_KEY")
dw2020 <- gs_key(mykey)

#list worksheets in the google sheet
gs_ws_ls(dw2020)


#read in all the data in the rally/event tab
events <- dw2020 %>% 
  gs_read(ws = "AK VER Rally / Event Tracker") %>% 
  clean_names() 


#set formatting of certain columns
events <- events %>% 
  mutate(
    date = mdy(date),
    thru_date = mdy(thru_date),
    cand_fullname = as.factor(cand_fullname),
    state = as.factor(state),
    cd_if_known = as.factor(cd_if_known),
    event_type = as.factor(event_type),
    cand_fullname = as.factor(cand_fullname),
    cand_lastname = as.factor(cand_lastname),
    sponsor = as.factor(sponsor)
  )


#filter for only future dates
events_upcoming <- events %>% 
  filter(date > Sys.Date(),
         date < (Sys.Date()+7)) %>% 
  mutate(location_geo = paste0(city, ", ", state)) %>% 
  select(cand_lastname, date, city, state, location_geo, event_type, description)


#remove NA's from city descriptions without a city
events_upcoming$location_geo <- str_remove(events_upcoming$location_geo, "NA, ")
  


#### STATE BY STATE SHADED MAP ####

# Downloading the shapefiles for states at the lowest resolution from tigris
states <- states(cb=T)

states %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)



upcoming_states <- events %>% 
  filter(date > Sys.Date(),
         date < (Sys.Date()+7)) %>% 
  group_by(state) %>%
  summarize(total=n()) 

upcoming_states <- upcoming_states %>% 
  mutate(total = as.integer(total))


# Now we use the Tigris function geo_join to bring together 
# are the two columns they'll be joined by

states_merged_sb <- geo_join(states, upcoming_states, "STUSPS", "state")

# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Greens", domain=states_merged_sb$total)

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work

states_merged_sb <- subset(states_merged_sb, !is.na(total))

# Setting up the pop up text
popup_sb <- paste0("Total: ", as.character(states_merged_sb$total))


# Mapping it with the new tiles CartoDB.Positron
l <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_sb , 
              fillColor = ~pal(states_merged_sb$total), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_sb) %>%
  addLegend(pal = pal, 
            values = states_merged_sb$total, 
            position = "bottomright", 
            title = "Trips")

l %>% 
  addTiles()



#alternative method -- hover popups
popup_sb <- paste0("Trips: ", as.character(states_merged_sb$total))

l2 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_sb , 
              fillColor = ~pal(states_merged_sb$total), 
              fillOpacity = 0.9, 
              weight = 0.2, 
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label=popup_sb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, 
            values = states_merged_sb$total, 
            position = "bottomright", 
            title = "Total trips")



l2 %>% 
  addTiles()


# 
# #### POINT MAP FOR CITIES ####
# 
# # https://www.jessesadler.com/post/geocoding-with-r/
# # https://community.rstudio.com/t/how-to-add-my-api-key-into-get-map/15992
# 
# #create this file locally in the same directory to store the api credentials for geocoder via google
# #when using git assign it to the gitignore list to avoid public dislcosure of the key
# source("geocodekey.R")
# 
# locs <- events_upcoming %>% 
#   filter(!is.na(city),
#          state != "INTL") %>% 
#   select(location_geo) %>% 
#   unique() 
# 
# locations_df <- mutate_geocode(locs, location_geo)
# locations_df 
# 
# #join the geocoded location table to the main upcoming events to add lat/lon
# joined_formap <- left_join(events_upcoming, locations_df) %>% 
#   mutate(cand_lastname = as.character(cand_lastname),
#          month = month(date),
#          day = day(date)
#   )
# 
# 
# #labels
# labs1 <- lapply(seq(nrow(joined_formap)), function(i) {
#   paste0( '<p>', 'Candidate: ', '<strong>', joined_formap[i, "cand_lastname"], '</strong></p>',
#           '<p></p>',
#           "City: ", joined_formap[i, "city"],
#           '<p></p>',
#           "Scheduled date: ", joined_formap[i, "month"], "/", joined_formap[i, "day"]
#   )
# })
# 
# m1 <- leaflet(joined_formap) %>% 
#   addTiles() %>%
#   addMarkers(lng = ~lon, lat = ~lat,
#              # radius = ~sqrt(amtcontrib) * 300,
#              # fillColor = ~pal(cmag_d_spotcnt),
#              # popup = lapply(labs1, HTML),
#              label = lapply(labs1, HTML),
#              clusterOptions = markerClusterOptions()
#   ) %>%
#   addControl("Upcoming candidate visits to confirmed cities", position = "topright") 
# # %>% 
# #   setView(-96, 37.8, zoom=4) 
# 
# m1



#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m1),'beto_contribs_byzip_points.html')





