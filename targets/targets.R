library(tidyverse)

player_pool <- read_csv("./results/hitter_projections.csv") %>% 
  bind_rows(read_csv("./results/pitcher_projections.csv")) 

undrafted <- filter(player_pool, is.na(status)) %>% 
  pull(Name)

data_for_reg <- player_pool %>%  
  select(Name, Team, position, dollar_value, NFBC_ADP) %>% 
  filter(!is.na(NFBC_ADP) & dollar_value > -2)  %>% 
  arrange(-dollar_value)




target_reg <- loess(NFBC_ADP ~ dollar_value^2, data = data_for_reg, span=0.5) 

smoothed10 <- predict(target_reg) 

plot(data_for_reg$NFBC_ADP, x=data_for_reg$dollar_value, 
     type="p", 
     main="Predicted ADP by Dollar Value", 
     xlab="Dollar Value", 
     ylab="ADP")
lines(smoothed10, x=data_for_reg$dollar_value, col="red")


target_list <- data_for_reg %>% 
  cbind(predicted_adp = smoothed10) %>%
  mutate(net_value = NFBC_ADP - predicted_adp) %>% 
  arrange(-net_value) %>% 
  filter(Name %in% undrafted) 

write_csv(target_list, path = "./results/sean_targets.csv") 
file.copy("./results/sean_targets.csv", "./shiny_fantasy/sean_targets.csv", overwrite = TRUE)
  
  