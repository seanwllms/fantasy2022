#get total dollars left
total_dollars_spent <- sum(draftpicks$salary)

total_dollars_remaining <- 270*18 - total_dollars_spent


total_value_remaining_hitters <- filter(hitter_projections, status != "drafted" & dollar_value >= 1) 
total_value_remaining_pitchers <- filter(pitcher_projections, status != "drafted" & dollar_value >= 1) 
total_value_remaining_hitters
total_value_remaining_pitchers
total_value_remaining <- sum(total_value_remaining_hitters$dollar_value - 1) + sum(total_value_remaining_pitchers$dollar_value - 1)
total_value_remaining

open_roster_spots <- rosters_merged %>% 
  filter(player =="" & !(position %in% BENCH_SPOTS)) %>% 
  nrow()

inflation_adjustment <- (total_dollars_remaining - open_roster_spots)/total_value_remaining

average_price_remaining <- total_dollars_remaining/open_roster_spots

#add inflation-adjusted price
hitter_projections <- hitter_projections %>% 
  mutate(auction_value = dollar_value * inflation_adjustment)

pitcher_projections <- pitcher_projections %>% 
  mutate(auction_value = dollar_value * inflation_adjustment)

save(total_dollars_spent,
     total_dollars_remaining,
     total_value_remaining,
     inflation_adjustment,
     average_price_remaining,
     open_roster_spots,
     file="./results/league_stats.rda")


file.copy("./results/league_stats.rda", "./shiny_fantasy/", overwrite = TRUE)