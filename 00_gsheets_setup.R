library(tidyverse)
library(lubridate)
library(janitor)
library(googlesheets)
library(gt)
library(ggmap)
library(plotly)
library(RColorBrewer)

#this will trigger a web page to authenticate with google account
gs_ls() 


#register DW's 2020 google sheet
mykey <- Sys.getenv("DW2020_KEY")
dw2020 <- gs_key(mykey)

#open in brower
# dw2020 %>% 
#   gs_browse()

#list worksheets in the google sheet
gs_ws_ls(dw2020)



### RALLY AND EVENTS ####

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


# build gt tables ####

# https://gt.rstudio.com/reference/index.html
events_selectcols %>% 
  gt() %>% 
  tab_header(
    title = "Campaign Events"
  ) %>% 
  # tab_spanner(
  #   label = "location",
  #   columns = vars(
  #     city, state)
  # ) %>% 
  tab_row_group(
    group = "Califorina Trips",
    rows = state == "California"
  ) %>%
  tab_row_group(
    group = "Non California Trips",
    rows = state != "California"
  ) 



events_selectcols %>% 
  gt() %>% 
  tab_header(
    title = "Campaign Events"
  ) %>% 
  # tab_spanner(
  #   label = "location",
  #   columns = vars(
  #     city, state)
  # ) %>% 
  tab_row_group(
    group = "Califorina Trips",
    rows = state == "California"
  ) %>%
  tab_row_group(
    group = "Non California Trips",
    rows = state != "California"
  ) 



## working space ####


datatable(events,
          rownames = FALSE,
          options = list(bPaginate = FALSE, 
                         searching = FALSE,
                         ordering = FALSE
          )) %>%
  formatDate('date', 'toDateString') 



events <- events %>% 
  mutate(
    date = mdy(date),
    thru_date = mdy(thru_date),
    cand_fullname = as.factor(cand_fullname),
    state = as.factor(state),
    cd_if_known = as.factor(cd_if_known),
    event_type = as.factor(event_type)
         )


events %>% 
  filter(date > Sys.Date(),
         date < (Sys.Date()+7))




#geocoding
# https://www.jessesadler.com/post/geocoding-with-r/
# https://community.rstudio.com/t/how-to-add-my-api-key-into-get-map/15992

locs <- events %>% 
  filter(!is.na(city)) %>% 
  mutate(location = paste0(city, ", ", state)) %>% 
  select(location) %>% 
  unique() %>% 
  head()


locations_df <- mutate_geocode(locs, location)


#########################################
### KEY REMARKS AND INVERVIEWS ##########

#read in all the data in the interviews/remarks tab
keyremarks <- dw2020 %>%
  gs_read(ws = "Key Interviews/Remarks") %>%
  clean_names()

keyremarks %>%
  count(candidate) %>%
  arrange(desc(n))

keyremarks %>%
  count(venue) %>%
  arrange(desc(n))




#### FACET CHARTS FOR CANDIDATES BY STATE ####


#by state counts
by_cand_and_state <- events %>% 
  filter(date < Sys.Date(),
         state != "INTL") %>% 
  count(cand_lastname, state) %>% 
  arrange(cand_lastname, desc(n)) 

#reorder factors to order bars descending
bystate <- bystate %>% 
  mutate(
    state = as.factor(state),
    state = fct_reorder(state, desc(n))
  )



colourCount = length(unique(by_cand_and_state$state))
getPalette = colorRampPalette(brewer.pal(9, "Set2"))


d <- ggplot(data = by_cand_and_state, aes(x = state, y = n, fill = state)) +
  geom_col() +
  # coord_flip() + 
  theme_minimal() +
  scale_fill_manual(values = getPalette(colourCount))
  # scale_fill_brewer(palette="Set3")

d

d2 <- d + labs(x ="", y = "") +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


d3 <- d2 + facet_wrap(~cand_lastname)

d3

dd <- ggplotly(d3) 

dd

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu



# https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
  

