################################################################
#################LEAGUE STUFF LIVES HERE########################
################################################################


#create data frame for each team.
# for (team in TEAMS) {
#       
#       assign(team,
#              data.frame(roster_spot = POSITIONS,
#                         salary = 0,
#                         Name = "",
#                         stringsAsFactors = FALSE
#              ),
#              env = .GlobalEnv
#       )
# }


rosters <- expand.grid(TEAMS, POSITIONS) %>% 
  set_names(c("team", "position")) %>% 
  mutate(position = factor(position, levels = POSITIONS)) %>% 
  arrange(team, position) %>% 
  map_df(as.character)