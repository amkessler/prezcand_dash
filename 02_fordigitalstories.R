library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(plotly)
library(googlesheets)
library(leaflet)
# library(ggmap)
library(RColorBrewer)
library(htmlwidgets)
library(widgetframe)



# to pull from live data

# #register DW's 2020 google sheet
# dw2020 <- gs_key("1rhCkWIHW3kblAP2H-1A7_123ACCuJLoafGXfnEldYl4")
# 
# #read in all the data in the rally/event tab
# events <- dw2020 %>% 
#   gs_read(ws = "AK VER Rally / Event Tracker") %>% 
#   clean_names() 


# to pull from saved temp file offline
events <- read_csv("events_temp.csv", 
                        col_types = cols(approx_date = col_character(), 
                                         cd_if_known = col_character(), date = col_character(), 
                                         source_or_link2 = col_character(), 
                                         thru_date = col_character()))



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



#by state counts
bystate <- events %>% 
  filter(date < Sys.Date()) %>% 
  count(state) %>% 
  arrange(desc(n)) %>% 
  head(10)

#reorder factors to order bars descending
bystate <- bystate %>% 
  mutate(
    state = as.factor(state),
    state = fct_reorder(state, desc(n))
  )


# by cand_fullname counts to date
bycand <- events %>%
  filter(date < Sys.Date()) %>% 
  count(cand_fullname) %>%
  arrange(n) %>% 
  head(10)

#reorder factors to order bars descending
bycand <- bycand %>%
  mutate(
    cand_fullname = as.factor(cand_fullname),
    cand_fullname = fct_reorder(cand_fullname, n)
  )



### PLOTS ####



# top states since midterms ####

d <- ggplot(data = bystate, aes(x = state, y = n)) +
  geom_col(
    # color = "#848484",
    fill = "#ffb3ba") + 
  # coord_flip() + 
  theme_minimal() +
  coord_flip()

d2 <- d + labs(x ="", y = "") +
  theme(legend.title=element_text("test")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dd <- ggplotly(d2) 

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu



#Candidates with most visits since midterm election ####

d <- ggplot(data = bycand, aes(x = cand_fullname, y = n)) +
  geom_col(
    # color = "#848484",
    fill = "#FFC966") +
  # coord_flip() +
  theme_minimal()

d2 <- d + labs(x ="", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # scale_fill_manual(values=cbPalette) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dd <- ggplotly(d2)

dd_nomenu <- dd %>% config(displayModeBar = FALSE)
dd_nomenu