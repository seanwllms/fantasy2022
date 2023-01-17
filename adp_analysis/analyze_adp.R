library(tidyverse)
library(ggplot2)


#read fangraphs data
adp_data_hitters <- read_tsv("./adp_analysis/hitteradp.tsv") %>% 
  rename(playername = 2,
         position = 4) %>%
  filter(!position == "P") %>% 
  select(playername, ADP)

adp_data_pitchers <- read_tsv("./adp_analysis/pitcheradp.tsv") %>% 
  rename(playername = 2) %>% 
  select(playername, ADP) %>% 
  filter(!playername == "Shohei Ohtani")


adp_data_all <- bind_rows(adp_data_hitters, adp_data_pitchers) %>% 
  mutate(name2 = strsplit(playername, ","),
         name2 = map(name2, set_names, c("last", "first"))) %>% 
  unnest_wider(name2) %>% 
  mutate(name_nfbc = paste(str_trim(first), last, sep =" ")) %>% 
  select(name_nfbc, ADP)

#sean values
hitters_sean <- read_csv("./results/hitter_projections.csv") %>% 
  select(Name, playerid, position, dollar_value, status)

pitchers_sean <- read_csv("./results/pitcher_projections.csv") %>% 
  select(Name, playerid, position, dollar_value, status) 

sean_dollar_values <- bind_rows(hitters_sean, pitchers_sean)  

#Read crosswalk
name_crosswalk <- read_xlsx("./adp_analysis/player_crosswalk.xlsx") %>% 
  select(IDFANGRAPHS, NFBCNAME) %>% 
  rename(playerid = IDFANGRAPHS,
         name_nfbc = NFBCNAME)


#merge fangraphs adp with sean dollar values
adp_analysis <- sean_dollar_values %>% 
  left_join(name_crosswalk)  %>% 
  left_join(adp_data_all) %>% 
  distinct()

adp_analysis %>% 
  rename(NFBC_ADP = ADP) %>% 
  select(playerid, NFBC_ADP) %>% 
  write_csv(path="./results/nfbc_adp.csv")

#create model to predict ADP from dollar_value
adp_model_data <- adp_analysis %>% 
  filter(ADP < 599 & dollar_value > -2) %>% 
  mutate(ADP2 = ADP^2,
         status = ifelse(is.na(status), "", status)) %>% 
  filter(Name != "Adalberto Mondesi") %>% 
  arrange(-dollar_value)


#plot adp vs. sean dollar values
adp_scatter <- adp_model_data %>% 
  ggplot(aes(x=dollar_value, y = ADP)) +
  geom_point() +
  geom_smooth()
adp_scatter


exponential_model <- loess(ADP ~ dollar_value, data=adp_model_data, span =.99)
smoothed <- predict(exponential_model)

plot(adp_model_data$ADP, 
     x=adp_model_data$dollar_value, 
     type="p", 
     main="Predicted ADP vs. Actual ADP", 
     xlab="Sean Estimated Dollar Value", 
     ylab="ADP")

lines(x = adp_model_data$dollar_value, y=smoothed)


#identify remaining values
sean_value_targets <- bind_cols(adp_model_data, model_adp=smoothed) %>% 
  mutate(value_targets = ADP - model_adp, dollar_value = round(dollar_value, 1)) %>% 
  filter(!(status=="drafted") & dollar_value > 8 & value_targets > 0) %>% 
  arrange(-value_targets) %>% 
  select(Name, position, dollar_value, ADP, model_adp)

write_csv(sean_value_targets, path="./results/sean_targets.csv")
file.copy("./results/sean_targets.csv", "./shiny_fantasy/sean_targets.csv", overwrite=TRUE)
