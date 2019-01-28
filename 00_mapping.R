library(tidyverse)
library(lubridate)
library(janitor)
library(googlesheets)
library(leaflet)
library(tidycensus)

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



events_upcoming <- events %>% 
  filter(date > Sys.Date()) %>% 
  mutate(location = paste0(city, ", ", state)) %>% 
  select(cand_lastname, date, location, event_type, description)




#some exploring 
events %>% 
  count(state) %>% 
  arrange(desc(n)) 

events %>% 
  count(cand_fullname) %>% 
  arrange(desc(n)) 

events_selectcols <- events %>% 
  select(date, cand_fullname, city, state, event_type, sponsor, description)


events %>% 
  filter(date < today("EST")) %>% 
  count(cand_fullname, state) %>% 
  arrange(cand_fullname, desc(n)) 