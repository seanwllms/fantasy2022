library(tidyverse)
library(readxl)

nfbc <- read_tsv("./NFBC/ADP.tsv") %>%
  select(-c(10:11)) %>% 
  set_names("Rank", "PlayerName","Team", "POS", "ADP", "min", "max", "dif", "picks") %>% 
  mutate(last_name = str_split(PlayerName, ",") %>% map_chr(.,1) ,
         first_name = str_split(PlayerName, ",") %>% map_chr(.,2) %>% map_chr(., str_trim),
         PlayerName = paste0(first_name," ", last_name),
         NFBC_ADP = ADP) %>% 
  select(PlayerName, NFBC_ADP)


fangraphs_names <- bind_rows(read_csv("./results/hitter_projections.csv"),
                             read_csv("./results/pitcher_projections.csv")) %>% 
  select(playerid, Name)


nfbc_crosswalk <- nfbc %>% 
  full_join(fangraphs_names, by=c("PlayerName" = "Name")) 
write_excel_csv(nfbc_crosswalk, "./NFBC/nfbc_match.csv")

errors <- nfbc %>% 
  anti_join(fangraphs_names, by=c("PlayerName" = "Name"))
write_excel_csv(errors, "./NFBC/nomatch.csv")


#add new data into nfbc crosswalk spreadsheet
updated_nfbc <- read_xlsx("./nfbc/nfbc_crosswalk.xlsx") %>% 
  select(-NFBC_ADP) %>% 
  left_join(nfbc, by = c("NFBC_Name" = "PlayerName")) %>% 
  select(playerid, NFBC_ADP) %>% 
  write_excel_csv("./results/nfbc_adp.csv")  
