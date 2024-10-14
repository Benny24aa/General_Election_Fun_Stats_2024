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
  filter(First.party != 'Spk', First.party != "Ind") %>% 
  rename(Party_Name = First.party)

# Total Number of Votes across all seats
general_election_total_electorate <- general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>%  
  summarise(Valid.votes = sum(Valid.votes), .groups = 'drop') %>% 
  select(-UK)


# Total Labour Vote Share
labour_final_vote_share <- general_election_data_set %>% 
  mutate(UK = "UK") %>% 
group_by(UK) %>% 
  summarise(Lab = sum(Lab), .groups = 'drop') %>% 
  select(-UK)%>% 
  rename(Total_Votes = Lab) %>% 
  mutate(Party_Name = "Lab")
  

# Total Conservative Vote Share
conservative_final_vote_Share <-general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>% 
  summarise(Con = sum(Con), .groups = 'drop') %>% 
  select(-UK)%>% 
  rename(Total_Votes = Con) %>% 
  mutate(Party_Name = "Con")

# Total Reform UK Vote Share
reform_final_vote_Share <-general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>% 
  summarise(RUK = sum(RUK), .groups = 'drop') %>% 
  select(-UK) %>% 
  rename(Total_Votes = RUK) %>% 
  mutate(Party_Name = "RUK")

# Total Liberal Democrat Vote Share

libdem_final_vote_Share <-general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>% 
  summarise(LD = sum(LD), .groups = 'drop') %>% 
  select(-UK) %>% 
  rename(Total_Votes = LD) %>% 
  mutate(Party_Name = "LD")


# Total Green Party Vote Share
green_final_vote_Share <-general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>% 
  summarise(Green = sum(Green), .groups = 'drop') %>% 
  select(-UK) %>% 
  rename(Total_Votes = Green) %>% 
  mutate(Party_Name = "Green")


# Total SNP Vote Share
SNP_final_vote_Share <-general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>% 
  summarise(SNP = sum(SNP), .groups = 'drop') %>% 
  select(-UK) %>% 
  rename(Total_Votes = SNP) %>% 
  mutate(Party_Name = "SNP")

# Total Vote Share PC (Wales)

pc_final_vote_Share <-general_election_data_set %>% 
  mutate(UK = "UK") %>% 
  group_by(UK) %>% 
  summarise(PC = sum(PC), .groups = 'drop') %>% 
  select(-UK) %>% 
  rename(Total_Votes = PC) %>% 
  mutate(Party_Name = "PC")

# Merging votes together using bind_rows 

party_vote_share_added <- bind_rows(pc_final_vote_Share,SNP_final_vote_Share, green_final_vote_Share, libdem_final_vote_Share, reform_final_vote_Share, conservative_final_vote_Share, labour_final_vote_share )

rm(pc_final_vote_Share,SNP_final_vote_Share, green_final_vote_Share, libdem_final_vote_Share, reform_final_vote_Share, conservative_final_vote_Share, labour_final_vote_share)

# Preparing data for ratio calculation of seat to vote share

general_election_result <- left_join(general_election_result, party_vote_share_added, by = "Party_Name")

rm(party_vote_share_added)

general_election_result <- general_election_result %>% 
  mutate(Number_Seat_Vote_Share = round(Total_Votes/Number_of_Seats)) %>% 
  mutate(Percentage_of_Vote = signif(Total_Votes/28809340 * 100, digits = 3))