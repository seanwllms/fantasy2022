hitter_positions <- c("C1","C2","1B","2B","SS","3B","CI","MI","OF1","OF2","OF3","OF4","OF5","OF6","DH")
pitcher_positions <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")



hitters_for_merge <- hitter_projections %>% 
  #filter(!(playerid %in% DUPLICATE_PLAYERS_TO_EXCLUDE)) %>% 
  select(Name, AB, R, HR, RBI, SB, AVG, OBP, marginal_total_points, dollar_value)

pitchers_for_merge <- pitcher_projections %>% 
  #filter(!(playerid %in% DUPLICATE_PLAYERS_TO_EXCLUDE)) %>% 
  select(Name, IP:K, marginal_total_points, dollar_value)


rosters_merged <- roster_draft %>% 
  left_join(hitters_for_merge, by = c("player" = "Name")) %>%
  left_join(pitchers_for_merge,  by = c("player" = "Name")) %>% 
  mutate(marginal_total_points = ifelse(str_detect(position, "P"),marginal_total_points.y, marginal_total_points.x),
         dollar_value = ifelse(str_detect(position, "P"), dollar_value.y, dollar_value.x)) %>% 
  select(-marginal_total_points.x, -dollar_value.x, -marginal_total_points.y, -dollar_value.y, -OBP) %>% 
  mutate(AB = ifelse(is.na(AB) & position %in% hitter_positions, 400, AB),
         R = ifelse(is.na(R)  & position %in% hitter_positions, pull(replacement_hitter, R), R),
         HR = ifelse(is.na(HR) & position %in% hitter_positions, pull(replacement_hitter, HR), HR),
         RBI = ifelse(is.na(RBI) & position %in% hitter_positions, pull(replacement_hitter, RBI), RBI),
         SB = ifelse(is.na(SB) & position %in% hitter_positions, pull(replacement_hitter, SB), SB),
         AVG = ifelse(is.na(AVG) & position %in% hitter_positions, pull(replacement_hitter, AVG), AVG),
         IP = ifelse(is.na(IP) &  position %in% pitcher_positions, 73, IP), 
         ERA = ifelse(is.na(ERA) & position %in% pitcher_positions, pull(replacement_pitcher, ERA), ERA),
         WHIP = ifelse(is.na(WHIP) & position %in% pitcher_positions, pull(replacement_pitcher, WHIP), WHIP),
         SV = ifelse(is.na(SV) & position %in% pitcher_positions, pull(replacement_pitcher, SV), SV),
         W = ifelse(is.na(W) & position %in% pitcher_positions, pull(replacement_pitcher, W), W),
         K = ifelse(is.na(K) & position %in% pitcher_positions, pull(replacement_pitcher, K), K)) 



#give catchers a lower replacmeent level
rosters_merged <- rosters_merged %>% 
  mutate(R = ifelse(position %in% c("C1", "C2") & player =="", 28, R),
         HR = ifelse(position %in% c("C1", "C2") & player =="", 9, HR),
         RBI = ifelse(position %in% c("C1", "C2") & player =="", 30, RBI),
         SB = ifelse(position %in% c("C1", "C2") & player =="", 1, SB),
         AVG = ifelse(position %in% c("C1", "C2") & player =="", .223, AVG),
         AB = ifelse(position %in% c("C1", "C2") & player =="", 260, AB))



