general_election_data_set <- read.csv("C:/Users/harle/OneDrive/Desktop/election analysis 2024/General_Election_Fun_Stats_2024/HoC-GE2024-results-by-constituency.csv")


#### North East of England Predictions

general_election_north_east_data <- general_election_data_set %>% 
  filter(Region.name == "North East")

#### Summary Data

last_time_north_east <- general_election_north_east_data %>% 
  select(Constituency.name,First.party, Second.party, Majority)

breakdown_constituency_north_east <- general_election_north_east_data %>% 
  select(Constituency.name, Lab, Con, RUK, LD, Green ) %>% 
  mutate(LabNew = round(Lab - Con*0.06 - RUK*0.22 - Green*0.006 - LD*0.002 -1500)) %>% 
  mutate(ReformNew = round(RUK + Lab*0.22 + Con*0.03 +1800)) %>% 
  mutate(ConNew = round(Con + Lab*0.06 - RUK*0.03+1000)) %>% 
  select(Constituency.name, LabNew, ReformNew, ConNew) %>% 
  rename(Lab = LabNew, Con = ConNew, RUK = ReformNew)

Calculation_decides_winner_north_east <- breakdown_constituency_north_east %>% 
  mutate(reformoverlab = Lab - RUK <0,
         reformovercon = Con - RUK <0,
         conoverlab = Lab - Con <0) %>% 
  mutate(Winning_Party = 
           case_when(reformoverlab == TRUE & reformovercon == TRUE & conoverlab == FALSE ~ "RUK", # First Reform, Second Labour, Third Conservative
                     reformoverlab == FALSE & reformovercon == TRUE & conoverlab == FALSE ~ "Lab", # First Labour, Second Reform, Third Conservative
                     reformoverlab == FALSE & reformovercon == FALSE & conoverlab == TRUE ~ 'Con', # First Conservative, Second Labour, Third Reform
                     reformoverlab == TRUE & reformovercon == FALSE & conoverlab == TRUE ~ 'Con' # First Conservative, Second Reform, Third Labour
           )) %>% 
  mutate(Second_Party = 
           case_when(reformoverlab == TRUE & reformovercon == TRUE & conoverlab == FALSE ~ "Lab", # First Reform, Second Labour, Third Conservative
                     reformoverlab == FALSE & reformovercon == TRUE & conoverlab == FALSE ~ "RUK", # First Labour, Second Reform, Third Conservative
                     reformoverlab == FALSE & reformovercon == FALSE & conoverlab == TRUE ~ 'Lab', # First Conservative, Second Labour, Third Reform
                     reformoverlab == TRUE & reformovercon == FALSE & conoverlab == TRUE ~ 'RUK' # First Conservative, Second Reform, Third Labour
             
             
             
           )) %>% 
  mutate(Third_Party =
           case_when(reformoverlab == TRUE & reformovercon == TRUE & conoverlab == FALSE ~ "Con", # First Reform, Second Labour, Third Conservative
                     reformoverlab == FALSE & reformovercon == TRUE & conoverlab == FALSE ~ "Con", # First Labour, Second Reform, Third Conservative
                     reformoverlab == FALSE & reformovercon == FALSE & conoverlab == TRUE ~ 'RUK', # First Conservative, Second Labour, Third Reform
                     reformoverlab == TRUE & reformovercon == FALSE & conoverlab == TRUE ~ 'Lab' # First Conservative, Second Reform, Third Labour
                     
           )) %>% 
  select(-reformoverlab, -reformovercon, -conoverlab)

north_east_last_time <- general_election_north_east_data %>% 
  select(Constituency.name, First.party ) %>% 
  rename(Winning_Party = First.party)

north_east_this_time <- Calculation_decides_winner_north_east %>% 
  select(Constituency.name, Winning_Party) %>% 
  rename(Winning_Party_Predict = Winning_Party)

merged_north_east_first_place <- full_join(north_east_last_time, north_east_this_time, by = "Constituency.name")


north_east_last_time_final_seat_count <-  north_east_last_time %>% 
  group_by(Winning_Party) %>% 
  summarise(count=n(), .groups = 'drop') %>% 
  rename(Number_of_Seats = count)

north_east_this_time_final_seat_count <- north_east_this_time %>% 
  group_by(Winning_Party_Predict) %>% 
  summarise(count=n(), .groups = 'drop') %>%
  rename(Winning_Party = Winning_Party_Predict) %>% 
  rename(Number_of_Seats_This_Time = count)

 Final_Seat_Count_North_East_Compare <- full_join(north_east_last_time_final_seat_count, north_east_this_time_final_seat_count, by = "Winning_Party")

 general_election_north_east_data_prediction <- general_election_data_set %>% 
   filter(Region.name == "North East") %>% 
   select(ONS.ID, ONS.region.ID, Constituency.name, Region.name, Country.name, Constituency.type, Electorate)
 
 general_election_north_east_data_prediction <- full_join(general_election_north_east_data_prediction, Calculation_decides_winner_north_east, by = "Constituency.name" )
 
 write_xlsx(general_election_north_east_data_prediction, "C:/Users/harle/OneDrive/Desktop/election analysis 2024/General_Election_Fun_Stats_2024/test.xlsx")