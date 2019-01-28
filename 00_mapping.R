library(tidyverse)
library(lubridate)
library(janitor)
library(googlesheets)
library(leaflet)
library(ggmap)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)

#this will trigger a web page to authenticate with google account
# gs_ls() %>% View()


#register DW's 2020 google sheet
dw2020 <- gs_key("1rhCkWIHW3kblAP2H-1A7_123ACCuJLoafGXfnEldYl4")

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
  filter(date > Sys.Date()) %>% 
  mutate(location_geo = paste0(city, ", ", state)) %>% 
  select(cand_lastname, date, city, state, location_geo, event_type, description)


#remove cities that are NA
events_upcoming$location_geo <- str_remove(events_upcoming$location_geo, "NA, ")
  


#### GEOCODING ON THE FLY ####

# https://www.jessesadler.com/post/geocoding-with-r/
# https://community.rstudio.com/t/how-to-add-my-api-key-into-get-map/15992

#create this file locally in the same directory to store the api credentials for geocoder via google
#when using git assign it to the gitignore list to avoid public dislcosure of the key
source("geocodekey.R")

locs <- events_upcoming %>% 
  filter(!is.na(city),
         state != "INTL") %>% 
  select(location_geo) %>% 
  unique() 

locations_df <- mutate_geocode(locs, location_geo)
locations_df 

#join the geocoded location table to the main upcoming events to add lat/lon
joined_formap <- left_join(events_upcoming, locations_df) %>% 
  mutate(cand_lastname = as.character(cand_lastname),
         month = month(date),
         day = day(date)
  )


#### MAPPING POINTS WITH LEAFLET #####

#labels
labs1 <- lapply(seq(nrow(joined_formap)), function(i) {
  paste0( '<p>', 'Candidate: ', '<strong>', joined_formap[i, "cand_lastname"], '</strong></p>',
          '<p></p>',
          "City: ", joined_formap[i, "city"],
          '<p></p>',
          "Scheduled date: ", joined_formap[i, "month"], "/", joined_formap[i, "day"]
  )
})

m1 <- leaflet(joined_formap) %>% 
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat,
             # radius = ~sqrt(amtcontrib) * 300,
             # fillColor = ~pal(cmag_d_spotcnt),
             # popup = lapply(labs1, HTML),
             label = lapply(labs1, HTML),
             clusterOptions = markerClusterOptions()
  ) %>%
  addControl("Upcoming candidate visits to confirmed cities", position = "topright") 
# %>% 
#   setView(-96, 37.8, zoom=4) 

m1

#save to frameable file for CMS
# htmlwidgets::saveWidget(frameableWidget(m1),'beto_contribs_byzip_points.html')




