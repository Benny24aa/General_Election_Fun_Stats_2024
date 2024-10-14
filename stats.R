library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(readr)
library(dplyr)
library("readxl")
library("RColorBrewer")
library(rsconnect)
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(readr)
library(readxl)
library(RColorBrewer)
library(stats)
library(utils)
library(sf)
library(writexl)
library(base64enc)
library(maps)
library(rgdal)
library(maptools)
library(sf)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(maps)
library(mapproj)
library(tmap)
library(tmaptools)
library(sf)
library(phsopendata)
library(plotly)
library(crosstalk)
library(lubridate)
library(kableExtra)
library(tidyverse)

# Data loaded in
general_election_data_set <- read.csv("C:/Users/harle/OneDrive/Desktop/election analysis 2024/General_Election_Fun_Stats_2024/HoC-GE2024-results-by-constituency.csv")

# General Election Seat Result 2024 - Main Parties
general_election_result <- general_election_data_set  %>% 
  filter(Region.name != 'Northern Ireland') %>% 
  select(First.party) %>% 
group_by(First.party) %>% 
  summarise(count=n(), .groups = 'drop') %>% 
  rename(Number_of_Seats = count) %>% 
  filter(First.party != 'Spk')


