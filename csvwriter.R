#write projections files to csv

view_team <- function(team_name) {
  
  rosters_merged %>% 
    filter(team == team_name) 
}

#marmaduke
view_team("marmaduke") %>% 
  write_csv("./results/marmaduke.csv")

#pasadena
view_team("pasadena") %>% 
  write_csv("./results/pasadena.csv")

#write roster csv to shiny folder
write_csv(rosters_merged, file="./shiny_fantasy/rosters.csv")

#get ADP
nfbc_adp <- read_csv("./results/nfbc_adp.csv") 

#get salaries
salaries <- rosters_merged %>% 
  select(player, salary) %>% 
  filter(player != "") %>% 
  rename(Name = player)

#hitter and pitcher projections
pitcher_projections %>% 
  left_join(nfbc_adp) %>% 
  left_join(salaries) %>%
  write_csv(file = "./results/pitcher_projections.csv")

hitter_projections %>% 
  select(playerid, Name, Team, position, PA, AB, R, HR, RBI, SB, AVG, marginal_total_points, dollar_value, auction_value, status) %>% 
  left_join(nfbc_adp) %>%
  left_join(salaries) %>% 
  write_csv(file = "./results/hitter_projections.csv") 


#create file for best remaining players
hitterpitcher <- bind_rows(hitter_projections, pitcher_projections) %>%
      arrange(desc(dollar_value)) %>%
      select(Name, Team, position, marginal_total_points, dollar_value,auction_value, status)

hitterpitcher <- filter(hitterpitcher, status != "drafted" & dollar_value > -5)

write.csv(hitterpitcher, "./results/bestremaining.csv")

#write out draft errors to csv
write.csv(drafterrors, "./results/drafterrors.csv")

#write standings output to file
standings.output <- select(standings,
                           team, spent, left, picks_left,
                           total_points, R_points, HR_points, RBI_points, 
                           SB_points, AVG_points, ERA_points, WHIP_points, K_points, SV_points, W_points)



write.csv(standings.output, file="./results/standings.csv")

#copy files needed for shiny app
files_to_copy <- c("./results/standings.csv", "./results/pitcher_projections.csv", "./results/hitter_projections.csv") 
walk(files_to_copy, file.copy, "./shiny_fantasy/", overwrite = TRUE)
